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
	has Parser:D $.type is required;
	has Sub:D $.converter = &null-converter;
	has Store:D $.store is required;
	has Any $.default;
	method store(Str:D $raw, Hash:D $hash) {
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
			my ($multi-class, $multi-args, $type, %options-args, $negatable) := $<argument>.ast;
			make $<names>.ast.map: -> $name {
				my $store = $multi-class.new(|%$multi-args, :key($<names>.ast[0]));
				my @options;
				@options.push: Option.new(:$name, :$store, :$type, :default, |%options-args);
				if $negatable {
					@options.push: Option.new(:name("no$name"), :$store, :$type, |%options-args, :!default);
					@options.push: Option.new(:name("no-$name"), :$store, :$type, |%options-args, :!default);
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
		{ make [ ScalarStore, {}, BooleanParser, {}, $<negatable> ] }
	}

	token counter {
		'+'
		{ make [ CountStore, {}, BooleanParser, { }, False ] }
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
		{ make [ %store-for{~$<repeat>}, { :type($<type>.ast.returns) }, ArgumentedParser, { :converter($<type>.ast) }, False ] }
	}

	token colon-type {
		':' <type>
		{ make [ ScalarStore, {}, MaybeArgumentedParser, { :converter($<type>.ast), :default($<type>.ast.returns.new) }, False ] }
	}

	token colon-int {
		':' $<num>=[<[0..9]>+]
		{ make [ ScalarStore, {}, MaybeArgumentedParser, { :converter(&int-converter), :default($<num>.Int) }, False ] }
	}

	token colon-count {
		':+'
		{ make [ CountStore, {}, MaybeArgumentedParser, { :converter(&int-converter), :default(1) }, False ] }
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
	(Str) => &null-converter,
	(Any) => &maybe-converter,
);

my sub parse-parameter(Parameter $param) {
	my ($key) = my @names = $param.named_names;
	my $type = $param.sigil eq '$' ?? $param.type !! $param.type.of;
	my $converter = %converter-for-type{$type} // &null-converter;
	my $store = %store-for{$param.sigil}.new(:$key, :$type);
	my $parser = $param.sigil eq '$' && $param.type === Bool ?? BooleanParser !! ArgumentedParser;
	my %args = $parser == BooleanParser ?? :default !! :$converter;
	return @names.map: -> $name {
		Option.new(:$name, :$store, :type($parser), |%args);
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
		if $!bundling && $head ~~ / ^ '-' $<values>=[\w <[\w-]>*] $ / -> $/ {
			my @values = $<values>.Str.comb;
			for @values.keys -> $index {
				my $value = @values[$index];
				my $option = %!options{$value} or die "No such option $value";
				given $option.type {
					when BooleanParser {
						$option.store-default(%hash);
					}
					when ArgumentedParser {
						if $index + 1 < @values.elems {
							$option.store($<values>.substr($index + 1), %hash);
						}
						elsif @args {
							$option.store(@args.shift, %hash);
						}
						else {
							die "No argument given for option $value";
						}
						last;
					}
					when MaybeArgumentedParser {
						if $index + 1 < @values.elems {
							$option.store($<values>.substr($index + 1), %hash);
						}
						elsif @args {
							$option.store(@args.shift, %hash);
						}
						else {
							$option.store-default(%hash);
						}
						last;
					}
				}
			}
		}
		elsif $head eq '--' {
			@list.append: |@args;
			last;
		}
		elsif $head ~~ / ^ '-' ** 1..2 $<name>=[\w <[\w-]>*] $ / -> $/ {
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
				die "Unknown option $<name>";
			}
		}
		elsif $!gnu-style && $head ~~ / ^ '--' $<name>=[<[\w-]>+] '=' $<value>=[.*] / -> $/ {
			if %!options{$<name>} -> $option {
				die "Option {$option.name} doesn't take arguments" if $option.type == BooleanParser;
				$option.store(~$<value>, %hash);
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

our sub MAIN_HELPER($retval = 0) is export {
	my &m = callframe(1).my<&MAIN>;
	return $retval unless &m;
	m(|Getopt::Long.new(&m).get-options(@*ARGS));
}
