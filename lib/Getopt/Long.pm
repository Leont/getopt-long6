use v6;

unit class Getopt::Long;

my sub null-converter(Str:D $value --> Str) {
	return $value;
}

my sub int-converter(Str:D $value --> Int) {
	return $value.Int;
}

my sub extended-int-converter(Str:D $value --> Int) {
	grammar Extended {
		token TOP {
			<sign> <bulk>
			{ make $<sign>.ast * $<bulk>.ast; }
		}

		token sign {
			$<char>=<[+-]>?
			{ make ($<char> eq '-' ?? -1 !! 1) }
		}
		token bulk {
			[ <hex> | <octal> | <binary> || <fallback> ]
			{ make $/.values[0].ast }
		}
		token hex {
			:i '0x' $<num>=[<[0..9A..F]>+]
			{ make :16(~$<num>) }
		}
		token octal {
			'0' $<num>=[<[0..7]>+]
			{ make :8(~$<num>) }
		}
		token binary {
			:i '0b' $<num>=[<[01]>+]
			{ make :2(~$<num>) }
		}
		token fallback {
			\d+
			{ make $/.Str.Int }
		}
	}
	return Extended.parse($value).ast // Int;
}

my sub rat-converter(Str:D $value --> Rat) {
	return $value.Rat;
}

my sub num-converter(Str:D $value --> Num) {
	return $value.Num;
}

my sub rat-num-converter(Str:D $value --> Real) {
	return val($value) ~~ Rat|Int ?? $value.Rat !! $value.Num;
}

my sub maybe-converter(Str:D $value --> Any) {
	return val($value);
}

my role Store {
	has Str:D $.key is required;
	has Any:U $.type = Any;
	method store($value, $hash) { ... }
}

my class ScalarStore does Store {
	method store(Any:D $value, Hash:D $hash) {
		$hash{$!key} = $value;
	}
}

my class CountStore does Store {
	method store(Any:D $value, Hash:D $hash) {
		$hash{$!key} += $value;
	}
}

my class ArrayStore does Store {
	method store(Any:D $value, Hash:D $hash) {
		$hash{$!key} //= Array[$!type].new;
		$hash{$!key}.push($value);
	}
}

my class HashStore does Store {
	method store(Any:D $pair, Hash:D $hash) {
		my ($key, $value) = $pair.split('=', 2);
		$hash{$!key} //= Hash[$!type].new;
		$hash{$!key}{$key} = $value;
	}
}

my class Option {
	has Str:D $.name is required;
	has Range:D $.arity is required;
	has Sub:D $.converter = &null-converter;
	has Store:D $.store is required;
	has Any $.default;
	method store(Any:D $raw, Hash:D $hash) {
		$!store.store($!converter($raw), $hash);
	}
	method store-default(Hash:D $hash) {
		$!store.store($!default, $hash);
	}
}

has Bool:D $!gnu-style is required;
has Bool:D $!permute is required;
has Bool:D $!bundling is required;
has Option:D %!options;

submethod BUILD(:$!gnu-style = True, :$!permute = False, :$!bundling = True, :%!options) { }

my %store-for = (
	'%' => HashStore,
	'@' => ArrayStore,
	''  => ScalarStore,
	'$' => ScalarStore,
);

my grammar Argument {
	token TOP {
		<names> <argument>
		{
			my ($multi-class, $multi-args, $arity, %options-args, $negatable) := $<argument>.ast;
			make $<names>.ast.map: -> $name {
				my $store = $multi-class.new(|%$multi-args, :key($<names>.ast[0]));
				my @options;
				@options.push: Option.new(:$name, :$store, :$arity, :default, |%options-args);
				if $negatable {
					@options.push: Option.new(:name("no$name"), :$store, :$arity, |%options-args, :!default);
					@options.push: Option.new(:name("no-$name"), :$store, :$arity, |%options-args, :!default);
				}
				|@options;
			}
		}
	}

	token names {
		[ $<name>=[<[\w-]>+ | '?'] ]+ % '|'
		{ make @<name>».Str.list }
	}

	token argument {
		[ <boolean> | <equals-more> | <equals> | <counter> | <colon-type> | <colon-int> | <colon-count> ]
		{ make $/.values[0].made }
	}

	token boolean {
		$<negatable>=['!'?]
		{ make [ ScalarStore, {}, 0..0, {}, $<negatable> ] }
	}

	token counter {
		'+'
		{ make [ CountStore, {}, 0..0, { }, False ] }
	}

	my %converter-for-format = (
		i => &int-converter,
		s => &null-converter,
		f => &rat-num-converter,
		r => &rat-converter,
		o => &extended-int-converter,
	);

	token type {
		<[sifor]>
		{ make %converter-for-format{$/} }
	}

	token equals {
		'=' <type> $<repeat>=[<[%@]>?]
		{ make [ %store-for{~$<repeat>}, { :type($<type>.ast.returns) }, 1..1, { :converter($<type>.ast) }, False ] }
	}

	rule range {
		| $<min=>=\d* ',' $<max>=\d*  { make ( +$<min> ) .. ($<max> ?? +$<max> !! *) }
		| $<num>=\d+                  { make $/.Int..$/.Int }
	}
	token equals-more {
		'=' <type> '{' <range>'}'
		{ make [ ArrayStore, { :type($<type>.ast.returns) }, $<range>.ast, { :converter($<type>.ast) }, False ] }
	}

	token colon-type {
		':' <type>
		{ make [ ScalarStore, {}, 0..1, { :converter($<type>.ast), :default($<type>.ast.returns.new) }, False ] }
	}

	token colon-int {
		':' $<num>=[<[0..9]>+]
		{ make [ ScalarStore, {}, 0..1, { :converter(&int-converter), :default($<num>.Int) }, False ] }
	}

	token colon-count {
		':+'
		{ make [ CountStore, {}, 0..1, { :converter(&int-converter), :default(1) }, False ] }
	}
}

multi method new(*@patterns, *%args) {
	my %options;
	for @patterns -> $pattern {
		if Argument.parse($pattern) -> $match {
			for @($match.ast) -> $option {
				%options{$option.name} = $option;
			}
		}
		else {
			die "Couldn't parse '$pattern'";
		}
	}
	return self.bless(|%args, :%options);
}

my %converter-for-type{Any:U} = (
	(Int) => &int-converter,
	(Rat) => &rat-converter,
	(Num) => &num-converter,
	(Str) => &null-converter,
	(Any) => &maybe-converter,
);

my sub parse-parameter(Parameter $param) {
	my ($key) = my @names = $param.named_names;
	my $type = $param.sigil eq '$' ?? $param.type !! $param.type.of;
	my $store = %store-for{$param.sigil}.new(:$key, :$type);
	my $type = $param.sigil eq '$' ?? $param.type !! $param.type.of;
	my $store = %store-for{$param.sigil}.new(:key(@names[0]), :$type);
	if $param.sigil eq '$' && $param.type === Bool {
		return @names.flatmap: -> $name {
			my @options;
			@options.push: Option.new(:$name, :$store, :arity(0..0), :default);
			if $param.default {
				@options.push: Option.new(:name("no$name") , :$store, :arity(0..0), :!default);
				@options.push: Option.new(:name("no-$name"), :$store, :arity(0..0), :!default);
			}
			@options;
		}
	}
	else {
		my $converter = %converter-for-type{$type} // &null-converter;
		return @names.map: -> $name {
			Option.new(:$name, :$store, :arity(1..1), :$converter);
		}
	}
}

multi method new(Sub $main, Bool:D :$gnu-style = True, Bool:D :$permute = False) {
	my %options;
	for $main.candidates -> $candidate {
		for $candidate.signature.params.grep(*.named) -> $param {
			for parse-parameter($param) -> $option {
				if %options{$option.name}:exists and %options{$option.name} !eqv $option {
					die "Can't merge arguments for {$option.name}";
				}
				%options{$option.name} = $option;
			}
		}
	}
	return self.bless(:$gnu-style, :$permute, :%options);
}

method get-options(@args is copy, :defaults(%hash) is copy) {
	my @list;
	while @args {
		my $head = @args.shift;

		my $consumed = 0;

		sub take-value($option, $value) {
			$option.store($value, %hash);
			$consumed++;
		}
		sub take-args($option) {
			while @args && $consumed < $option.arity.min {
				$option.store(@args.shift, %hash);
				$consumed++;
			}

			while @args && $consumed < $option.arity.max && !@args[0].starts-with('--') {
				$option.store(@args.shift, %hash);
				$consumed++;
			}

			if $consumed == 0 && $option.arity.min == 0 {
				$option.store-default(%hash);
			}
			elsif $consumed < $option.arity.min {
				die "No argument given for option {$option.name}";
			}
		}

		if $!bundling && $head ~~ / ^ '-' $<values>=[\w <[\w-]>*] $ / -> $/ {
			my @values = $<values>.Str.comb;
			for @values.keys -> $index {
				my $value = @values[$index];
				my $option = %!options{$value} or die "No such option $value";
				if $option.arity.max > 0 && $index + 1 < @values.elems {
					take-value($option, $<values>.substr($index + 1));
				}

				take-args($option);
				last if $consumed;
			}
		}
		elsif $head eq '--' {
			@list.append: |@args;
			last;
		}
		elsif $head ~~ / ^ '-' ** 1..2 $<name>=[\w <[\w-]>*] $ / -> $/ {
			if %!options{$<name>} -> $option {
				take-args($option);
			}
			else {
				die "Unknown option $<name>";
			}
		}
		elsif $!gnu-style && $head ~~ / ^ '--' $<name>=[<[\w-]>+] '=' $<value>=[.*] / -> $/ {
			if %!options{$<name>} -> $option {
				die  "$<name> doesn't take arguments" if $option.arity.max == 0;
				take-value($option, ~$<value>);

				take-args($option);
			}
			else {
				die "Unknown option $<name>";
			}
		}
		else {
			if $!permute {
				@list.push: $head;
			}
			else {
				@list.append: $head, |@args;
				last;
			}
		}
	}
	return \(|@list, |%hash);
}

our sub get-options-from(@args, *@options, :%defaults, *%config) is export {
	my $getopt = Getopt::Long.new(|@options, |%config);
	return $getopt.get-options(@args, :%defaults);
}

our sub get-options(*@options, :%defaults, *%config) is export {
	return get-options-from(@*ARGS, |@options, :%defaults, |%config);
}

my package EXPORT::DEFAULT {
	if $*PERL.compiler.version after 2018.06 {
		OUR::{'&MAIN_HELPER'} := anon sub MAIN_HELPER(Bool $in-is-args, $retval = 0) {
			my $main = callframe(1).my<&MAIN>;
			return $retval unless $main;
			my $capture = Getopt::Long.new($main).get-options(@*ARGS);
			if $in-is-args {
				my $in := $*IN;
				my $*ARGFILES := IO::ArgFiles.new($in, :nl-in($in.nl-in), :chomp($in.chomp), :encoding($in.encoding), :bin(!$in.encoding));
				$main(|$capture);
			}
			else {
				$main(|$capture);
			}
		}
	}
	else {
		OUR::{'&MAIN_HELPER'} := anon sub MAIN_HELPER($retval = 0) {
			my $main = callframe(1).my<&MAIN>;
			return $retval unless $main;
			my $capture = Getopt::Long.new($main).get-options(@*ARGS);
			$main(|$capture);
		}
	}
}

=begin pod

=head1 NAME

Getopt::Long

=head1 SYNOPSIS

  use Getopt::Long;
  my $capture = get-options("length=i" # numeric
              "file=s"    # string
              "verbose"); # flag
  if $capture<verbose> { ... }

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

To use Getopt::Long from a Perl6 program, you must include the
following line in your program:

    use Getopt::Long;

This will load the core of the Getopt::Long module and prepare your
program for using it.

=head2 Simple options

The most simple options are the ones that take no values. Their mere
presence on the command line enables the option. Popular examples are:

    --all --verbose --quiet --debug

Handling simple options is straightforward:

    sub MAIN(Bool :$verbose, Bool :$all) { ... }

or:

    my $options = get-options('verbose', 'all');

The call to C<get-options()> parses the command line arguments that are
present in C<@*ARGS> and sets the option variable to the value C<True>
if the option did occur on the command line. Otherwise, the option
variable is not touched. Setting the option value to true is often
called I<enabling> the option.

The option name as specified to the get-options() function is called
the option I<specification>. Later we'll see that this specification
can contain more than just the option name.

get-options() will return a C<Capture> if the command line could be
processed successfully. Otherwise, it will throw an error using
die().

=head2 A little bit less simple options

Getopt::Long supports two useful variants of simple options:
I<negatable> options and I<incremental> options.

A negatable option is specified with an exclamation mark C<!> after the
option name or a default value for C<MAIN> argument:

    sub MAIN(Bool :$verbose = False) { ... }

or:

    my $options = get-options('verbose!');

Now, using C<--verbose> on the command line will enable
C<< $options<verbose> >>, as expected. But it is also allowed to use
C<--noverbose> or C<--no-verbose>, which will disable
C<< $options<verbose> >> by setting its value to C<False>.

An incremental option is specified with a plus C<+> after the
option name:

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
passed to the program unharmed, in C<@ARGV>.

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

    my $options = get-options('library=s@');

Used with the example above, C<$options<library>> would
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

    get-options('coordinates=f{2}', 'rgbcolor=i{3}');

It is also possible to specify the minimal and maximal number of
arguments an option takes. C<foo=s{2,4}> indicates an option that
takes at least two and at most 4 arguments. C<foo=s{1,}> indicates one
or more values; C<foo:s{,}> indicates zero or more option values.

=head2 Options with hash values

If you specify that the option can have multiple named values by
adding a "%":

    sub MAIN(Str :%define) { ... }

or

    my $options = get-options("define=s%");

When used with command line options:

    --define os=linux --define vendor=redhat

the hash C<< $options<define> >> will contain two keys, C<"os">
with value C<"linux"> and C<"vendor"> with value C<"redhat">. It is
also possible to specify that only integer or floating point numbers
are acceptable values. The keys are always taken to be strings.

=head2 Options with multiple names

Often it is user friendly to supply alternate mnemonic names for
options. For example C<--height> could be an alternate name for
C<--length>. Alternate names can be included in the option
specification, separated by vertical bar C<|> characters. To implement
the above example:

    sub MAIN(:height(:$length)) { ... }

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

Using negation on a single letter option when bundling is in effect is
pointless and will result in a warning.

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

String. An arbitrary sequence of characters. It is valid for the
argument to start with C<-> or C<-->.

=end item2

=begin item2
i

Integer. An optional leading plus or minus sign, followed by a
sequence of digits.

=end item2

=begin item2
o

Extended integer, Perl style. This can be either an optional leading
plus or minus sign, followed by a sequence of digits, or an octal
string (a zero, optionally followed by '0', '1', .. '7'), or a
hexadecimal string (C<0x> followed by '0' .. '9', 'a' .. 'f', case
insensitive), or a binary string (C<0b> followed by a series of '0'
and '1').

=end item2

=begin item2
r

Rational number. For example C<3.14>.

=end item2

=begin item2
f

Real number. For example C<3.14>, C<-6.23E24> and so on.

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
    my $p = Getopt::Long.new(@options);
    my $o = $p.get-options(@args) ...

Configuration options can be passed to the constructor as named
arguments:

    $p = Getopt::Long.new(@options, :!permute);

=head2 Parsing options from an arbitrary array

By default, get-options parses the options that are present in the
global array C<@*ARGV>. A special entry C<get-options-from> can be
used to parse options from an arbitrary array.

    use Getopt::Long;
    $ret = get-options-from(@myargs, ...);

The following two calls behave identically:

    $ret = get-options( ... );
    $ret = get-options-from(@*ARGS, ... );

=head2 Bundling

With bundling it is possible to set several single-character options
at once. For example if C<a>, C<v> and C<x> are all valid options,

    -vax

will set all three.

Getopt::Long supports three styles of bundling. To enable bundling, a
call to Getopt::Long::Configure is required.

Configured this way, single-character options can be bundled but long
options B<must> always start with a double dash C<--> to avoid
ambiguity. For example, when C<vax>, C<a>, C<v> and C<x> are all valid
options,

    -vax

will set C<a>, C<v> and C<x>, but

    --vax

will set C<vax>.

=head1 Configuring Getopt::Long

C<get-options> and C<get-options-from> take the following named options
to configure.

=begin item
gnu_compat

C<gnu_compat> controls whether C<--opt=> is allowed, and what it should
do. Without C<gnu_compat>, C<--opt=> gives an error. With C<gnu_compat>,
C<--opt=> will give option C<opt> and empty value.
This is the way GNU getopt_long() does it.

Note that C<--opt value> is still accepted, even though GNU
getopt_long() doesn't.

=end item

=begin item
permute (default:disabled)

Whether command line arguments are allowed to be mixed with options.
Default is disabled.

If C<permute> is enabled, this means that

    --foo arg1 --bar arg2 arg3

is equivalent to

    --foo --bar arg1 arg2 arg3

=end item

=begin item
bundling (default: enabled)

Enabling this option will allow single-character options to be
bundled. To distinguish bundles from long option names, long options
I<must> be introduced with C<--> and bundles with C<->.

Note that, if you have options C<a>, C<l> and C<all>, , possible
arguments and option settings are:

    using argument   sets option(s)
    -------------------------------
    -a, --a          a
    -l, --l          l
    -all             a, l
    --all            all

=end item

=begin item
gnu-style (default: enabled)

C<gnu-style> controls whether C<--opt=> is allowed, and what it should do. Without C<gnu-styl>, C<--opt=> gives an error. With C<gnu_compat>, C<--opt=> will give option opt and empty value. This is the way GNU getopt_long() does it.

Note that C<--opt value> is still accepted.

=end item

=head1 Return values and Errors

C<get-options> returns a capture to indicate success.
Configuration errors and errors in the option definitions are
signalled using die() and will terminate the calling program unless
caught by exception handling.

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
