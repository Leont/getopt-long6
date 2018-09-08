use v6;

unit class Getopt::Long;

my enum Parser < BooleanParser ArgumentedParser MaybeArgumentedParser >;

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

my sub maybe-converter(Str:D $value --> Any) {
	return $value ~~ / ^ '-'? \d+ $/ ?? IntStr($value.Int, $value) !! $value;
}

my role Multiplexer {
	has Str:D $.key is required;
	has Any:U $.type = Any;
	method store($value, $hash) { ... }
}

my class Monoplexer does Multiplexer {
	method store(Any:D $value, Hash:D $hash) {
		$hash{$!key} = $value;
	}
}

my class Countplexer does Multiplexer {
	method store(Any:D $value, Hash:D $hash) {
		$hash{$!key} += $value;
	}
}

my class Arrayplexer does Multiplexer {
	method store(Any:D $value, Hash:D $hash) {
		$hash{$!key} //= Array[$!type].new;
		$hash{$!key}.push($value);
	}
}

my class Hashplexer does Multiplexer {
	method store(Any:D $pair, Hash:D $hash) {
		my ($key, $value) = $pair.split('=', 2);
		$hash{$!key} //= Hash[$!type].new;
		$hash{$!key}{$key} = $value;
	}
}

my class Option {
	has Str:D $.name is required;
	has Parser:D $.type is required;
	has Sub:D $.converter = &null-converter;
	has Multiplexer:D $.multiplexer is required;
	has Any $.default;
	method store(Str:D $raw, Hash:D $hash) {
		$!multiplexer.store($!converter($raw), $hash);
	}
	method store-default(Hash:D $hash) {
		$!multiplexer.store($!default, $hash);
	}
}

has Bool:D $.gnu-style is required;
has Bool:D $.permute is required;
has Option:D %!options is required;

submethod BUILD(:$!gnu-style, :$!permute, :%!options) { }

my %multiplexer-for = (
	'%' => Hashplexer,
	'@' => Arrayplexer,
	''  => Monoplexer,
	'$' => Monoplexer,
);

my grammar Argument {
	token TOP {
		<names> <argument>
		{
			my ($multi-class, $multi-args, $type, %options-args, $negatable) := $<argument>.ast;
			make $<names>.ast.map: -> $name {
				my $multiplexer = $multi-class.new(|%$multi-args, :key($<names>.ast[0]));
				my @options;
				@options.push: Option.new(:$name, :$multiplexer, :$type, :default, |%options-args);
				if $negatable {
					@options.push: Option.new(:name("no$name"), :$multiplexer, :$type, |%options-args, :!default);
					@options.push: Option.new(:name("no-$name"), :$multiplexer, :$type, |%options-args, :!default);
				}
				|@options;
			}
		}
	}

	token names {
		[ $<name>=[\w+] ]+ % '|'
		{ make @<name>».Str.list }
	}

	token argument {
		[ <boolean> | <equals> | <counter> | <colon-type> | <colon-int> | <colon-count> ]
		{ make $/.values[0].made }
	}

	token boolean {
		$<negatable>=['!'?]
		{ make [ Monoplexer, {}, BooleanParser, {}, $<negatable> ] }
	}

	token counter {
		'+'
		{ make [ Countplexer, {}, BooleanParser, { }, False ] }
	}

	my %converter-for-format = (
		i => &int-converter,
		s => &null-converter,
		f => &rat-converter,
		o => &extended-int-converter,
	);

	token type {
		<[sifo]>
		{ make %converter-for-format{$/} }
	}

	token equals {
		'=' <type> $<repeat>=[<[%@]>?]
		{ make [ %multiplexer-for{~$<repeat>}, { :type($<type>.ast.returns) }, ArgumentedParser, { :converter($<type>.ast) }, False ] }
	}

	token colon-type {
		':' <type>
		{ make [ Monoplexer, {}, MaybeArgumentedParser, { :converter($<type>.ast), :default($<type>.ast.returns.new) }, False ] }
	}

	token colon-int {
		':' $<num>=[<[0..9]>+]
		{ make [ Monoplexer, {}, MaybeArgumentedParser, { :converter(&int-converter), :default($<num>.Int) }, False ] }
	}

	token colon-count {
		':+'
		{ make [ Countplexer, {}, MaybeArgumentedParser, { :converter(&int-converter), :default(1) }, False ] }
	}
}

multi method new(*@patterns, Bool:D :$gnu-style = True, Bool:D :$permute = False) {
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
	return self.bless(:$gnu-style, :$permute, :%options);
}

my %converter-for-type{Any:U} = (
	(Int) => &int-converter,
	(Rat) => &rat-converter,
	(Str) => &null-converter,
	(Any) => &maybe-converter,
);

my sub parse-parameter(Parameter $param) {
	my ($key) = my @names = $param.named_names;
	my $type = $param.sigil eq '$' ?? $param.type !! $param.type.of;
	my $converter = %converter-for-type{$type} // &null-converter;
	my $multiplexer = %multiplexer-for{$param.sigil}.new(:$key, :$type);
	my $parser = $param.sigil eq '$' && $param.type === Bool ?? BooleanParser !! ArgumentedParser;
	my %args = $parser == BooleanParser ?? :default !! :$converter;
	return @names.map: -> $name {
		Option.new(:$name, :$multiplexer, :type($parser), |%args);
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

method get-options(@argv) {
	my @args = @argv;
	my (@list, %hash);
	while @args {
		my $head = @args.shift;
		if $head ~~ / ^ '--' $<name>=[<[\w-]>+] $ / -> $/ {
			if %!options{$<name>} -> $option {
				given $option.type {
					when BooleanParser {
						$option.store-default(%hash);
					}
					when ArgumentedParser {
						if @args {
							$option.store(@args.shift, %hash);
						}
						else {
							die "No argument given for $<name>";
						}
					}
					when MaybeArgumentedParser {
						if @args {
							$option.store(@args.shift, %hash);
						}
						else {
							$option.store-default(%hash);
						}
					}
				}
			}
			else {
				die "Unknown option $<name>: ";
			}
		}
		elsif $!gnu-style && $head ~~ / ^ '--' $<name>=[<[\w-]>+] '=' $<value>=[.*] / -> $/ {
			if %!options{$<name>} -> $option {
				die "Option {$option.name} doesn't take arguments" unless $option.type == ArgumentedParser|MaybeArgumentedParser;
				$option.store(~$<value>, %hash);
			}
			else {
				die "Unknown option $<name>";
			}
		}
		elsif $head ~~ / ^ '-' $<values>=[\w .*] $ / -> $/ {
			my @values = $<values>.Str.comb;
			my @options = @values.map: -> $value { %!options{$value} };
			if all(@options).defined && all(@options).type == BooleanParser {
				for @options -> $option {
					$option.store-default(%hash);
				}
			}
			else {
				if @options[0] && @options[0].type == ArgumentedParser|MaybeArgumentedParser {
					if @values > 1 {
						@options[0].store($<values>.substr(1), %hash);
					}
					elsif @args {
						@options[0].store(@args.shift, %hash);
					}
				}
			}
		}
		elsif $head eq '--' {
			@list.append: |@args;
			last;
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

our sub MAIN_HELPER($retval = 0) is export {
	my &m = callframe(1).my<&MAIN>;
	return $retval unless &m;
	m(|Getopt::Long.new(&m).get-options(@*ARGS));
}
