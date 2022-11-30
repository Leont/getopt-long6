use v6;
use fatal;

unit class Getopt::Long:ver<0.3.5>;

class Exception is CORE::Exception {
	has Str:D $.message is required;
	method new(Str $message) {
		return self.bless(:$message);
	}
}

role FormattableException is CORE::Exception {
	has Str:D $.format is required;
	method new(Str $format) {
		return self.bless(:$format);
	}
	method rethrow-with(Str $name) {
		die Exception.new($!format.sprintf($name));
	}
	method message() {
		$!format.sprintf('some');
	}
}

class ValueInvalid does FormattableException {
}

class ConverterInvalid does FormattableException {
}

sub convert(Str:D $value, Code:D $converter) {
	return $converter($value);
	CATCH {
		when X::Str::Numeric {
			die ValueInvalid.new(qq{Cannot convert %s argument "$value" to number: $_.reason()});
		}
		when X::Numeric::CannotConvert {
			die ValueInvalid.new("Cannot convert %s argument $_.source() to {$_.target // $_.target.perl}: $_.reason()");
		}
		when X::Temporal {
			die ValueInvalid.new(.message.subst(/'string ' ( \' .* \' ) <.before ';'> /, { "$0 given as %s argument" }));
		}
		when FormattableException {
			.rethrow;
		}
		default {
			die ValueInvalid.new("Can not convert %s argument \"$value\": {.message}");
		}
	}
}

my role Store {
	has Str:D $.key is required;
	has Code:D $.converter = *.self;
	has Junction:D $.constraints = all();
	has Hash $.values is required;
	method check-constraints(Any:D $value) {
		die ValueInvalid.new(qq{Can't accept %s argument "$value" because it fails its constraints}) unless $value ~~ $!constraints;
	}
	method store-convert(Str:D $value) {
		self.store-direct(convert($value, $!converter));
	}
	method store-direct(Any:D $value) { ... }
}

my class ScalarStore does Store {
	method store-direct(Any:D $value) {
		self.check-constraints($value);
		$!values{$!key} = $value;
	}
}

my class CountStore does Store {
	method store-direct(Int:D $value) {
		$!values{$!key} += $value;
	}
}

my class ArrayStore does Store {
	has Any:U $.type is required;
	method store-direct(Any:D $value) {
		self.check-constraints($value);
		$!values{$!key} //= $!type === Any ?? Array !! Array[$!type].new;
		$!values{$!key}.push($value);
	}
}

my class HashStore does Store {
	has Any:U $.type is required;
	method store-convert(Any:D $pair) {
		my ($key, $value) = $pair.split('=', 2);
		my $converted-value = convert($value, $!converter);
		self.check-constraints($converted-value);
		$!values{$!key} //= $!type === Any ?? Hash !! Hash[$!type].new;
		$!values{$!key}{$key} = $converted-value;
	}
	method store-direct(Any:D $pair) {
		!!!
	}
}

my class Receiver {
	has Range:D $.arity is required;
	has Store:D $.store is required;
	has Any $.default;
	method store(Any:D $raw) {
		$!store.store-convert($raw);
	}
	method store-default() {
		$!store.store-direct($!default);
	}
}

sub get-converter(Any:U $type) {
	state %converter-for-type{Any:U} = (
		Pair.new(Int,      *.Int),
		Pair.new(Rat,      *.Rat),
		Pair.new(Num,      *.Num),
		Pair.new(Real,     *.Real),
		Pair.new(Numeric,  *.Numeric),
		Pair.new(Complex,  *.Complex),
		Pair.new(Str,      *.Str),
		Pair.new(IO::Path, *.IO),
		Pair.new(IO,       *.IO),
		Pair.new(DateTime, *.DateTime),
		Pair.new(Date,     *.Date),
		Pair.new(Version,  *.Version),
		Pair.new(Any,      &val),
	);

	state $coercion-how = try ::("Metamodel::CoercionHOW");
	if %converter-for-type{$type} -> &converter {
		return &converter;
	} elsif $type.HOW ~~ $coercion-how {
		my &primary = get-converter($type.^constraint_type());
		return sub coercion-converter(Str $input) {
			return $type.^coerce(primary($input));
		}
	} elsif $type.HOW ~~ Metamodel::EnumHOW {
		my $valid-values = $type.WHO.keys.sort({ $type.WHO{$^value} }).join(", ");
		return sub enum-converter(Str $value) {
			return $type.WHO{$value} // $type.^enum_from_value($value) // die ValueInvalid.new(qq{Can't convert %s argument "$value" to $type.^name(), valid values are: $valid-values});
		}
	} else {
		die ConverterInvalid.new("No argument conversion known for %s argument (type {$type.^name})");
	}
}

role Argument {
	has Junction:D $.constraints = all();
}

role Argument::Valued does Argument {
	has Any:U $.type = Str;
	has Code:D $.converter = get-converter($!type);
}

class Argument::Boolean does Argument {
	has Bool:D $.negatable = False;
}
multi make-receivers(Argument::Boolean $arg, Str:D $key, @names, %values) {
	my $store = ScalarStore.new(:$key, :constraints($arg.constraints), :%values);
	gather for @names -> $name {
		take $name => Receiver.new(:$store, :arity(0..0), :default);
		if $arg.negatable {
			take "no$name" => Receiver.new(:$store, :arity(0..0), :!default);
			take "no-$name" => Receiver.new(:$store, :arity(0..0), :!default);
		}
	}
}

class Argument::Scalar does Argument::Valued {
	has Any $.default;
}
multi make-receivers(Argument::Scalar $arg, Str:D $key, @names, %values) {
	my $store = ScalarStore.new(:$key, :converter($arg.converter), :constraints($arg.constraints), :%values);
	my $arity = $arg.default.defined ?? 0..1 !! 1..1;
	return @names.map: { $^name => Receiver.new(:$store, :$arity, :default($arg.default)) }
}

class Argument::Array does Argument::Valued {
	has Range:D $.arity = 1..1;
}
multi make-receivers(Argument::Array $arg, Str:D $key, @names, %values) {
	my $store = ArrayStore.new(:$key, :type($arg.type), :converter($arg.converter), :constraints($arg.constraints), :%values);
	return @names.map: { $^name => Receiver.new(:$store, :arity($arg.arity)) }
}

class Argument::Hash does Argument::Valued {
}
multi make-receivers(Argument::Hash $arg, Str:D $key, @names, %values) {
	my $store = HashStore.new(:$key, :type($arg.type), :converter($arg.converter), :constraints($arg.constraints), :%values);
	return @names.map: { $^name => Receiver.new(:$store, :arity(1..1)) }
}

class Argument::Counter does Argument {
	has Bool:D $.argumented = False;
}
multi make-receivers(Argument::Counter $arg, Str:D $key, @names, %values) {
	my $store = CountStore.new(:$key, :converter(*.Int), :constraints($arg.constraints), :%values);
	my $arity = $arg.argumented ?? 0..1 !! 0..0;
	return @names.map: { $^name => Receiver.new(:$store, :$arity, :1default) }
}

my rule name { [\w+]+ % '-' | '?' }

class Option {
	has Str @.names is required;
	has Str:D $.key is required;
	has Argument $.argument;
	submethod TWEAK(:@names) {
		die Exception.new('No name given for option') if @names < 1;
		die Exception.new("Invalid name(s): @names[]") if any(@names) !~~ &name;
	}
	multi method new(:@names!, :$argument!, Str :$key = @names[0]) {
		return self.bless(:@names, :$key, :$argument);
	}
	multi method new(:$name!, :$argument!) {
		return self.bless(:names[$name], :key($name), :$argument);
	}
}

has Code:D @!positionals is built;
has Option:D @!options is built;

method !positionals {
	return @!positionals.map(*.returns);
}

method !options {
	return @!options;
}

my Str @ordinals = <first second third fourth fifth sixth seventh eighth nineth tenth some some> ... *;

sub converter-for-positional($type, $ordinal) {
	CATCH { when ConverterInvalid { .rethrow-with($ordinal); }}
	return get-converter($type);
}

method new-from-objects(Getopt::Long:U: @options, :positionals(@raw-positionals)) {
	my @positionals = @raw-positionals Z[&converter-for-positional] @ordinals;
	return self.new(:@options, :@positionals);
}

my %argument-for = (
	'%' => Argument::Hash,
	'@' => Argument::Array,
	'$' => Argument::Scalar,
	''  => Argument::Scalar,
);

my sub type-for-format(Str:D $format) {
	state %type-for-format =
		i => Int,
		o => Int, # compatability with p5
		s => Str,
		f => Num,
		r => Rat,
		c => Complex,
		p => IO::Path,
		d => DateTime,
		a => Date,
		v => Version;
	die ConverterInvalid.new("No such format $format for %s") if not %type-for-format{$format}:exists;
	return %type-for-format{$format};
};

my grammar Parser {
	token TOP {
		<names> <argument>
		{ make Option.new(:names($<names>.ast), :argument($<argument>.ast)); }
	}

	token names {
		<name>+ % '|'
		{ make @<name>».Str.list }
	}

	token argument {
		[ <boolean> | <equals-more> | <equals> | <counter> | <colon-type> | <colon-int> | <colon-count> ]
		{ make $/.values[0].made }
	}

	token boolean {
		$<negatable>=['!'?]
		{ make Argument::Boolean.new(:negatable(?$<negatable>)) }
	}

	token counter {
		'+'
		{ make Argument::Counter.new }
	}

	token type {
		<alpha>
		{ make type-for-format(~$/) }
	}

	token equals {
		'=' <type> $<repeat>=[<[%@]>?]
		{ make %argument-for{~$<repeat>}.new(:type($<type>.ast)) }
	}

	rule range {
		| $<min=>=\d* ',' $<max>=\d*  { make ( +$<min> ) .. ($<max> ?? +$<max> !! *) }
		| $<num>=\d+                  { make $/.Int..$/.Int }
	}
	token equals-more {
		'=' <type> '{' <range>'}'
		{ make Argument::Array.new(:type($<type>.ast), :arity($<range>.ast)) }
	}

	token colon-type {
		':' <type>
		{ make Argument::Scalar.new(:type($<type>.ast), :default($<type>.ast.new)) }
	}

	token colon-int {
		':' $<num>=[<[0..9]>+]
		{ make Argument::Scalar.new(:type(Int), :default($<num>.Int)) }
	}

	token colon-count {
		':+'
		{ make Argument::Counter.new(:argumented) }
	}
}

our sub parse-option(Str $pattern) {
	CATCH { when ConverterInvalid { .rethrow-with("pattern $pattern") }}
	with Parser.parse($pattern) -> $match {
		return $match.ast;
	} else {
		die Exception.new("Couldn't parse argument specification '$pattern'");
	}
}

method new-from-patterns(Getopt::Long:U: @patterns, Str:D :$positionals = "") {
	my @objects = @patterns.map(&parse-option);
	my @positionals = $positionals.comb.map(&type-for-format);
	return self.new-from-objects(@objects, :@positionals);
}

my role Parsed {
	has Getopt::Long:D $.getopt is required;
}

multi sub trait_mod:<is>(Sub $sub, Bool :$getopt!) is export(:DEFAULT, :traits) {
	return $sub does Parsed(Getopt::Long.new-from-sub($sub));
}
multi sub trait_mod:<is>(Sub $sub, :@getopt!) is export(:DEFAULT, :traits) {
	return $sub does Parsed(Getopt::Long.new-from-patterns(@getopt));
}
multi sub trait_mod:<is>(Sub $sub, Getopt::Long:D :$getopt!) is export(:DEFAULT, :traits) {
	return $sub does Parsed($getopt);
}

my role Formatted {
	has Argument $.argument is required;
}

multi sub trait_mod:<is>(Parameter $param, Argument :option($argument)!) is export(:DEFAULT, :traits) {
	return $param does Formatted(:$argument);
}

our sub parse-argument(Str $pattern, Str $name) {
	CATCH { when ConverterInvalid { .rethrow-with("parameter {$name}") }}
	with Parser.parse($pattern, :rule('argument')) -> $match {
		return $match.ast;
	} else {
		die Exception.new("Couldn't parse parameter $name\'s argument specification '$pattern'");
	}
}

multi sub trait_mod:<is>(Parameter $param, Str:D :getopt(:$option)!) is export(:DEFAULT, :traits) {
	my $argument = parse-argument($option, $param.named_names[0]);
	return $param does Formatted(:$argument);
}

multi sub trait_mod:<is>(Parameter $param, Code:D :option($converter)!) is export(:DEFAULT, :traits) {
	my $element-type = $param.sigil eq '@'|'%' ?? $param.type.of !! $param.type;
	my $type = $element-type ~~ Any ?? $element-type !! Any;
	my $argument = %argument-for{$param.sigil}.new(:$type, :$converter);
	return $param does Formatted(:$argument);
}

multi get-argument(Parameter $param) {
	if $param.sigil eq '$' {
		my $type = $param.type;
		my $constraints = $param.constraints;
		if $type === Bool {
			return Argument::Boolean.new(:$constraints, :negatable(?$param.default));
		} else {
			return Argument::Scalar.new(:$type, :$constraints);
		}
	} else {
		my $type = $param.type.of ~~ Any ?? $param.type.of !! Any;
		return %argument-for{$param.sigil}.new(:$type);
	}
	CATCH { when ConverterInvalid {
		.rethrow-with("parameter {$param.name}");
	}}
}
multi get-argument(Parameter $param where Formatted) {
	return $param.argument;
}

sub make-option(Parameter $param) {
	return Option.new(:names($param.named_names), :argument(get-argument($param)));
}

multi get-option-objects(&candidate) {
	return &candidate.signature.params.grep(*.named).map(&make-option);
}
multi get-option-objects(&candidate where Parsed) {
	return &candidate.getopt!options;
}

multi get-positionals(&candidate) {
	return &candidate.signature.params.grep(*.positional).map(*.type);
}
multi get-positionals(&candidate where Parsed) {
	return &candidate.getopt!positionals;
}

sub get-positional-types(Sub $main) {
	my @positional-types = $main.candidates.map(&get-positionals);
	my $elem-max = max(@positional-types».elems);
	gather for ^$elem-max -> $index {
		my @types = @positional-types.grep(* > $index)»[$index];
		die Exception.new("Positional arguments are of different types {@types.perl}") unless [===] @types;
		take converter-for-positional(@types[0], @ordinals[$index]);
	}
}

method new-from-sub(Getopt::Long:U: Sub $main) {
	my @options = $main.candidates.flatmap(&get-option-objects);
	my @positionals = get-positional-types($main);
	return self.new(:@options, :@positionals);
}

method get-options(Getopt::Long:D: @args is copy, :%hash, :$auto-abbreviate = False, :$compat-builtin = False, :named-anywhere(:$permute) = !$compat-builtin, :$bundling = !$compat-builtin, :$compat-singles = $compat-builtin, :$compat-negation = $compat-builtin, :$compat-positional = $compat-builtin, :$compat-space = $compat-builtin, :$auto-help = $compat-builtin, :$write-args) {
	my @list;

	sub to-receivers(Option $option) {
		CATCH { when ConverterInvalid { .rethrow-with("--{$option.names[0]}") }}
		return make-receivers($option.argument, $option.key, $option.names, %hash);
	}
	my %receivers = @!options.flatmap(&to-receivers);

	while @args {
		my $head = @args.shift;

		my $consumed = 0;

		sub get-receiver(Str:D $key, Str:D $name) {
			with %receivers{$key} -> $option {
				return $option;
			} elsif $key eq 'help' && $auto-help {
				return Receiver.new(:store(ScalarStore.new(:key<help>)), :arity(0..0), :default);
			} elsif $auto-abbreviate {
				my @names = %receivers.keys.grep(*.starts-with($key));
				if @names == 1 {
					return %receivers{ @names[0] };
				} elsif @names > 1 {
					die Exception.new("Ambiguous partial option $name, possible interpretations: @names[]");
				} else {
					die Exception.new("Unknown option $name");
				}
			} else {
				die Exception.new("Unknown option $name");
			}
		}

		sub take-value(Receiver:D $receiver, Str:D $value, Str:D $name) {
			CATCH { when ValueInvalid { .rethrow-with($name) } }
			$receiver.store($value);
			$consumed++;
		}
		sub take-args(Receiver:D $receiver, Str:D $name) {
			while @args && $consumed < $receiver.arity.min {
				take-value($receiver, @args.shift, $name);
			}

			while !$compat-space && @args && $consumed < $receiver.arity.max && !@args[0].starts-with('--') {
				take-value($receiver, @args.shift, $name);
			}

			if $consumed == 0 && $receiver.arity.min == 0 {
				$receiver.store-default();
			} elsif $consumed < $receiver.arity.min {
				die Exception.new("The option $name requires a value but none was specified");
			}
		}

		if $bundling && $head ~~ / ^ '-' $<values>=[\w .* ] $ / -> $/ {
			my @values = $<values>.Str.comb;
			for @values.keys -> $index {
				my $value = @values[$index];
				my $receiver = get-receiver($value, "-$value");
				if $receiver.arity.max > 0 && $index + 1 < @values.elems {
					my $offset = $compat-singles && @values[$index + 1] eq '=' ?? 2 !! 1;
					take-value($receiver, $<values>.substr($index + $offset), "-$value");
				}

				take-args($receiver, "-$value");
				last if $consumed;
			}
		}
		elsif $compat-singles && $head ~~ / ^ '-' <name> '=' $<value>=[.*] / -> $/ {
			my $receiver = get-receiver(~$<name>, "-$<name>");
			die Exception.new("Option -$<name> doesn't take an argument") if $receiver.arity.max != 1;
			take-value($receiver, ~$<value>, "-$<name>");
		}
		elsif $head eq '--' {
			@list.append: |@args;
			last;
		}
		elsif $head ~~ / ^ '-' ** 1..2 <name> $ / -> $/ {
			take-args(get-receiver(~$<name>, ~$/), ~$/);
		}
		elsif $head ~~ / ^ $<full-name>=[ '--' <name> ] '=' $<value>=[.*] / -> $/ {
			my $receiver = get-receiver(~$<name>, ~$<full-name>);
			die Exception.new("Option $<full-name> doesn't take arguments") if $receiver.arity.max == 0;
			take-value($receiver, ~$<value>, ~$<full-name>);
			take-args($receiver, ~$<full-name>);
		}
		elsif $compat-negation && $head ~~ / ^ $<full-name>=[ '-' ** 1..2 '/' <name> ] ['=' $<value>=[.*]]?  $ / {
			if $<value> {
				my $receiver = get-receiver(~$<name>, ~$<full-name>);
				die Exception.new("Option $<full-name> doesn't take an argument") if $receiver.arity.max != 1;
				take-value($receiver, ~$<value> but False, ~$<full-name>);
			} else {
				take-args(get-receiver('no-' ~ $<name>, ~$<full-name>), ~$<full-name>);
			}
		} else {
			if $permute {
				@list.push: $head;
			} else {
				@list.append: $head, |@args;
				last;
			}
		}
	}

	my &fallback-converter = $compat-positional ?? &val !! *.self;
	my @converters = |@!positionals, &fallback-converter, *;
	my @positionals = (@list Z @ordinals Z @converters).map: -> $ [ $value, $name, $converter ] {
		CATCH { when ValueInvalid { .rethrow-with($name) }}
		convert($value, $converter);
	};

	@$write-args = @list if $write-args;
	return \(|@positionals, |%hash);
}

our sub get-options-from(@args, *@elements, :$overwrite, *%config) is export(:DEFAULT, :functions) {
	my %hash := @elements && @elements[0] ~~ Hash ?? @elements.shift !! {};
	my @options;
	for @elements -> $element {
		when $element ~~ Str {
			@options.push: $element;
		}
		when $element ~~ Pair {
			my $key = $element.key;
			my ($name) = $element.key ~~ / ^ (\w+) /[0];
			%hash{$name} := $element.value;
			given $element.value {
				when Positional {
					$key ~= '@' unless $key.ends-with('@'|'}');
				}
				when Associative {
					$key ~= '%' unless $key.ends-with('%');
				}
			}
			@options.push: $key;
		}
		default {
			die Exception.new("Unknown element type: " ~ $element.perl);
		}
	}
	my $getopt = Getopt::Long.new-from-patterns(@options);
	return $getopt.get-options(@args, |%config, :%hash, :write-args($overwrite ?? @args !! Any));
}

our sub get-options(|args) is export(:DEFAULT, :functions) {
	return get-options-from(@*ARGS, :overwrite, |args);
}

our sub call-with-getopt(&func, @args, %options = %*SUB-MAIN-OPTS // {}) is export(:DEFAULT, :functions) {
	my $capture = Getopt::Long.new-from-sub(&func).get-options(@args, |%options, :write-args(@args));
	return func(|$capture);
}

our sub ARGS-TO-CAPTURE(Sub $func, @args) is export(:DEFAULT, :MAIN) {
	my %options = %*SUB-MAIN-OPTS // {};
	return Getopt::Long.new-from-sub($func).get-options(@args, |%options, :write-args(@args));
	CATCH { when Exception { note .message; &*EXIT(2) } };
}

=begin pod

=head1 NAME

Getopt::Long

=head1 SYNOPSIS

  use Getopt::Long;
  get-options("length=i" => my $length, # numeric
              "file=s"   => my $file,   # string
              "verbose"  => my $verbose); # flag

  use Getopt::Long;
  my $options = get-options("length=i", # numeric
                            "file=s",   # string
                            "verbose"); # flag

or

 use Getopt::Long;
 sub MAIN(Int :$length, Str :$file, Bool :$verbose) { ... }

=head1 DESCRIPTION

The Getopt::Long module implements extended getopt functions called
C<get-options()> and C<get-options-from>, as well as automatic argument
parsing for a C<MAIN> sub.

This function adheres to the POSIX syntax for command
line options, with GNU extensions. In general, this means that options
have long names instead of single letters, and are introduced with a
double dash "--". Support for bundling of command line options, as was
the case with the more traditional single-letter approach, is also
provided.

=head1 Command Line Options, an Introduction

Command line operated programs traditionally take their arguments from
the command line, for example filenames or other information that the
program needs to know. Besides arguments, these programs often take
command line I<options> as well. Options are not necessary for the
program to work, hence the name 'option', but are used to modify its
default behaviour. For example, a program could do its job quietly,
but with a suitable option it could provide verbose information about
what it did.

Command line options come in several flavours. Historically, they are
preceded by a single dash C<->, and consist of a single letter.

    -l -a -c

Usually, these single-character options can be bundled:

    -lac

Options can have values, the value is placed after the option
character. Sometimes with whitespace in between, sometimes not:

    -s 24 -s24

Due to the very cryptic nature of these options, another style was
developed that used long names. So instead of a cryptic C<-l> one
could use the more descriptive C<--long>. To distinguish between a
bundle of single-character options and a long one, two dashes are used
to precede the option name. Also, option values could be specified
either like

    --size=24

or

    --size 24

=head1 Getting Started with Getopt::Long

To use Getopt::Long from a Raku program, you must include the
following line in your program:

    use Getopt::Long;

This will load the core of the Getopt::Long module and prepare your
program for using it.

=head2 Getopt::Long as a MAIN wrapper

Getopt::Long can be used as a argument parsing MAIN wrapper, replacing
the builtin argument parsing. It will by default offer a Unix-typical
command line interface, but various options allow it to be more similar
to Raku's ideosyncratic parsing.

It supports the following types for named and positional arguments:

=item Bool
=item Any
=item Str
=item Int
=item Rat
=item Num
=item Real
=item Numeric
=item Complex
=item IO::Path
=item DateTime
=item Date
=item Version

It also supports any enum type, and any coercion type that uses any of
the aforementioned types as its contraint type (e.g. C<Foo(Str)>).

An explicit converter can also be set using an `is option` trait, e.g.

 sub MAIN(Foo :$foo is option(&foo-converter)) { ... }

=head2 Simple options

The most simple options are the ones that take no values. Their mere
presence on the command line enables the option. Popular examples are:

    --all --verbose --quiet --debug

Handling simple options is straightforward:

    sub MAIN(Bool :$verbose, Bool :$all) { ... }

or:

    get-options('verbose' => my $verbose, 'all' => my $all);

The call to C<get-options()> parses the command line arguments that are
present in C<@*ARGS> and sets the option variable to the value C<True>
if the option did occur on the command line. Otherwise, the option
variable is not touched. Setting the option value to true is often
called I<enabling> the option.

The option name as specified to the C<get-options()> function is called
the option I<specification>. Later we'll see that this specification
can contain more than just the option name.

C<get-options()> will return a C<Capture> if the command line could be
processed successfully. Otherwise, it will throw an error using
die().

=head2 A little bit less simple options

Getopt::Long supports two useful variants of simple options:
I<negatable> options and I<incremental> options.

A negatable option is specified with an exclamation mark C<!> after the
option name or a default value for C<MAIN> argument:

    sub MAIN(Bool :$verbose = False) { ... }

or:

    get-options('verbose!' => my $verbose);

or:

    my $options = get-options('verbose!');

Now, using C<--verbose> on the command line will enable
C<$verbose>, as expected. But it is also allowed to use
C<--noverbose> or C<--no-verbose>, which will disable
C<< $verbose >> by setting its value to C<False>.

An incremental option is specified with a plus C<+> after the
option name:

    sub MAIN(Int :$verbose is option('+')) { ... }

or:

   get-options('verbose+' => my $verbose);

or

    my $options = get-options('verbose+');

Using C<--verbose> on the command line will increment the value of
C<$verbose>. This way the program can keep track of how many times the
option occurred on the command line. For example, each occurrence of
C<--verbose> could increase the verbosity level of the program.

=head2 Mixing command line option with other arguments

Usually programs take command line options as well as other arguments,
for example, file names. It is good practice to always specify the
options first, and the other arguments last. Getopt::Long will,
however, allow the options and arguments to be mixed and 'filter out'
all the options before passing the rest of the arguments to the
program. To stop Getopt::Long from processing further arguments,
insert a double dash C<--> on the command line:

    --size 24 -- --all

In this example, C<--all> will I<not> be treated as an option, but
passed to the program unharmed, in C<@*ARGS>.

=head2 Options with values

For options that take values it must be specified whether the option
value is required or not, and what kind of value the option expects.

Three kinds of values are supported: integer numbers, floating point
numbers, and strings.

If the option value is required, Getopt::Long will take the
command line argument that follows the option and assign this to the
option variable. If, however, the option value is specified as
optional, this will only be done if that value does not look like a
valid command line option itself.

    sub MAIN(Str :$tag) { ... }

or

    get-options('tag=s' => my $tag);

or
    my %options = get-options('tag=s');

In the option specification, the option name is followed by an equals
sign C<=> and the letter C<s>. The equals sign indicates that this
option requires a value. The letter C<s> indicates that this value is
an arbitrary string. Other possible value types are C<i> for integer
values, and C<f> for floating point values. Using a colon C<:> instead
of the equals sign indicates that the option value is optional. In
this case, if no suitable value is supplied, string valued options get
an empty string C<''> assigned, while numeric options are set to C<0>.

=head2 Options with multiple values

Options sometimes take several values. For example, a program could
use multiple directories to search for library files:

    --library lib/stdlib --library lib/extlib

You can specify that the option can have multiple values by adding a
"@" to the format, or declare the argument as positional:

    sub MAIN(Str :@library) { ... }

or

    get-options('library=s@' => my @libraries);

or

    my $options = get-options('library=s@');

Used with the example above, C<@libraries>/C<$options<library>> would
contain two strings upon completion: C<"lib/stdlib"> and
C<"lib/extlib">, in that order. It is also possible to specify that
only integer or floating point numbers are acceptable values.

Warning: What follows is an experimental feature.

Options can take multiple values at once, for example

    --coordinates 52.2 16.4 --rgbcolor 255 255 149

This can be accomplished by adding a repeat specifier to the option
specification. Repeat specifiers are very similar to the C<{...}>
repeat specifiers that can be used with regular expression patterns.
For example, the above command line would be handled as follows:

    my $options = get-options('coordinates=f{2}', 'rgbcolor=i{3}');

or

    sub MAIN(Rat :@coordinates is option('f{2}'),
      Int :@rgbcolor is option('i{3}'))


    get-options('coordinates=f{2}' => my @coordinates,
      'rgbcolor=i{3}' => my @rgbcolor);

It is also possible to specify the minimal and maximal number of
arguments an option takes. C<foo=s{2,4}> indicates an option that
takes at least two and at most 4 arguments. C<foo=s{1,}> indicates one
or more values; C<foo:s{,}> indicates zero or more option values.

=head2 Options with hash values

If you specify that the option can have multiple named values by
adding a "%":

    sub MAIN(Str :%define) { ... }

or

    get-options("define=s%" => my %define);

or

    my $options = get-options("define=s%");

When used with command line options:

    --define os=linux --define vendor=redhat

the hash C<%defines> or C<< $options<define> >> will contain two keys,
C<"os"> with value C<"linux"> and C<"vendor"> with value C<"redhat">.
It is also possible to specify that only integer or floating point
numbers are acceptable values. The keys are always taken to be strings.

=head2 Options with multiple names

Often it is user friendly to supply alternate mnemonic names for
options. For example C<--height> could be an alternate name for
C<--length>. Alternate names can be included in the option
specification, separated by vertical bar C<|> characters. To implement
the above example:

    sub MAIN(:height(:$length)) { ... }

or

    get-options('length|height=f' => my $length);

or

    $options = get-options('length|height=f');

The first name is called the I<primary> name, the other names are
called I<aliases>. When using a hash to store options, the key will
always be the primary name.

Multiple alternate names are possible.

=head2 Summary of Option Specifications

Each option specifier consists of two parts: the name specification
and the argument specification.

The name specification contains the name of the option, optionally
followed by a list of alternative names separated by vertical bar
characters.

    length            option name is "length"
    length|size|l     name is "length", aliases are "size" and "l"

The argument specification is optional. If omitted, the option is
considered boolean, a value of C<True> will be assigned when the option is
used on the command line.

The argument specification can be

=begin item
!

The option does not take an argument and may be negated by prefixing
it with "no" or "no-". E.g. C<"foo!"> will allow C<--foo> (a value of
1 will be assigned) as well as C<--nofoo> and C<--no-foo> (a value of
0 will be assigned). If the option has aliases, this applies to the
aliases as well.

=end item

=begin item
+

The option does not take an argument and will be incremented by 1
every time it appears on the command line. E.g. C<"more+">, when used
with C<--more --more --more>, will increment the value three times,
resulting in a value of 3 (provided it was 0 or undefined at firs).

The C<+> specifier is ignored if the option destination is not a scalar.

=end item

=begin item
= I<type> [ I<desttype> ] [ I<repeat> ]

The option requires an argument of the given type. Supported types
are:

=begin item2
s

String(C<Str>). An arbitrary sequence of characters. It is valid for the
argument to start with C<-> or C<-->.

=end item2

=begin item2
i

Integer (C<Int>). This can be either an optional leading plus or minus sign,
followed by a sequence of digits, or an octal string (C<0o>, optionally
followed by '0', '1', .. '7'), or a hexadecimal string (C<0x> followed
by '0' .. '9', 'a' .. 'f', case insensitive), or a binary string (C<0b>
followed by a series of '0' and '1').

=end item2

=begin item2
r

Rational number (C<Rat>). For example C<3.14>.

=end item2

=begin item2
f

Floating-pointer number (C<Num>). For example C<3.14>, C<-6.23E24> and so on.

=end item2

=begin item2
c

Complex number (C<Complex>). For example C<1+2i>.

=end item2

=begin item2
p

Path (C<IO::Path>). For example C<foo/bar.txt>.

=end item2

=begin item2
d

An ISO-8601 formatted date and time (C<DateTime>). For example C<2019-12-30T01:23:45-0700>.

=end item2

=begin item2
a

A ISO-8601 formatted date (C<Date>). For example C<2019-12-30>.

=end item2

=begin item2
v

A Version (C<Version>). For example C<1.2.3>.

=end item2

The I<desttype> can be C<@> or C<%> to specify that the option is
list or a hash valued.

The I<repeat> specifies the number of values this option takes per
occurrence on the command line. It has the format
C<{> [ I<min> ] [ C<,> [ I<max> ] ] C<}>.

I<min> denotes the minimal number of arguments. It defaults to C<0>.

I<max> denotes the maximum number of arguments. It must be at least
I<min>. If I<max> is omitted, I<but the comma is not>, there is no
upper bound to the number of argument values taken.

=end item

=begin item
: I<type> [ I<desttype> ]

Like C<=>, but designates the argument as optional.
If omitted, an empty string will be assigned to string values options,
and the value zero to numeric options.

Note that if a string argument starts with C<-> or C<-->, it will be
considered an option on itself.

=end item

=begin item
: I<number> [ I<desttype> ]

Like C<:i>, but if the value is omitted, the I<number> will be assigned.

=end item

=begin item
: + [ I<desttype> ]

Like C<:i>, but if the value is omitted, the current value for the
option will be incremented.

=end item

=head1 Advanced Possibilities

=head2 Object oriented interface

Getopt::Long can be used in an object oriented way as well:

    use Getopt::Long;
    my $p = Getopt::Long.new-from-patterns(@options);
    my $o = $p.get-options(@args) ...

Configuration options can be passed to the constructor as named
arguments:

    $p = Getopt::Long.new-from-patterns(@options, :!permute);

=head2 Parsing options from an arbitrary array

By default, get-options parses the options that are present in the
global array C<@*ARGS>. A special entry C<get-options-from> can be
used to parse options from an arbitrary array.

    use Getopt::Long;
    $ret = get-options-from(@myargs, ...);

The following two calls behave identically:

    $ret = get-options( ... );
    $ret = get-options-from(@*ARGS, :overwrite, ... );

=head1 Configuring Getopt::Long

C<get-options> and C<get-options-from> take the following named options
to configure. When using Getopt::Long as a C<MAIN> wrapper, you can set
them using the C<%*SUB-MAIN-OPTS> variable:

=begin item
auto-abbreviate (default: C<False>)

Enabling this allows option names to be abbreviated to uniqueness (e.g.
`--foo` can be written as `--f` if no other option starts with an `f`).

=end item

=begin item
compat-builtin (default: C<False>)

Enable all compatibility options that make argument parsing more like
the builtin argument parsing. Currently that means disabling C<bundling>
and C<permute>, and enabling C<compat-singles>, C<compat-negation>,
C<compat-positional> and C<auto-help>)

=end item

=begin item
bundling (default: C<!$compat-builtin>)

Enabling this option will allow single-character options to be
bundled. To distinguish bundles from long option names, long options
I<must> be introduced with C<--> and bundles with C<->.

Note that, if you have options C<a>, C<l> and C<all>, possible
arguments and option settings are:

    using argument   sets option(s)
    -------------------------------
    -a, --a          a
    -l, --l          l
    -all             a, l
    --all            all

=end item

=begin item
permute (default: C<!$compat-builtin>)

Whether command line arguments are allowed to be mixed with options.
Default is enabled unless C<$compat-builtin> is set.

If C<permute> is enabled, this means that

    --foo arg1 --bar arg2 arg3

is equivalent to

    --foo --bar arg1 arg2 arg3

=end item

=begin item
compat-singles (default: C<$compat-builtin>)

Enabling this will allow single letter arguments with an C<=> between
the letter and its argument. E.g. C<-j=2> instead of C<-j2>. This is for
compatibility with raku's built-in argument parsing.

=end item

=begin item
compat-negation (default: C<$compat-builtin>)

Enabling this will allow one to one to use C<--/foo> as an alias for
C<--no-foo>, for compatibility with raku's built-in argument parsing.
Note that this still requires the presence of a C<--no-foo> handler,
typically by using the C<!> modifier.

=end item

=begin item
compat-positional (default: C<$compat-builtin>)

Enabling this will turn all positional arguments into allomorphs, if
possible.

=end item

=begin item
compat-space (default: C<$compat-builtin>)

By default, an option with an optional argument will take that as a
separate argument unless that argument starts with C<-->; e.g.
C<--foo bar>. If this option is enabled, no such separate arguments are
allowed, and the only way to express such an argument is in the same
argument: C<--foo=bar>.

=end item

=begin item
auto-help (default: C<$compat-builtin>)

This adds an extra --help option, that can hook into Raku's built-in
usage message generator.

=end item

=head1 Return values and Errors

C<get-options> returns a capture to indicate success, or throws an
C<Getopt::Long::Exception> otherwise.

=head1 Troubleshooting

=head2 C<get-options> does not fail when an option is not supplied

That's why they're called 'options'.

=head2 C<get-options> does not split the command line correctly

The command line is not split by get-options, but by the command line
interpreter (CLI). On Unix, this is the shell. On Windows, it is
CMD.EXE. Other operating systems have other CLIs.

It is important to know that these CLIs may behave different when the
command line contains special characters, in particular quotes or
backslashes. For example, with Unix shells you can use single quotes
(C<'>) and double quotes (C<">) to group words together. The following
alternatives are equivalent on Unix:

 "two words"
 'two words'
 two\ words

In case of doubt, insert the following statement in front of your Perl
program:

 note @*ARGS.join('|');

to verify how your CLI passes the arguments to the program.

=head1 AUTHOR

Leon Timmermans <fawaka@gmail.com>

=end pod
