use v6;

unit class Getopt::Long;

role Parser {
	has Str:D $.name is required;
	has Str:D @.aliases = ();
	method takes-argument(--> Bool) { ... }
	method names() {
		return ($!name, |@!aliases);
	}
	method parse($raw, $others) { ... }
}

class BooleanParser does Parser {
	has Bool $.negatable;
	method takes-argument(--> Bool) {
		return False;
	}
	method parse($raw, $others) {
		my @names = $.names;
		if / ^ @names $/ {
			return True;
		}
		elsif $!negatable && / ^ 'no' '-'? @names $/ {
			return False
		}
	}
}

class ArgumentedParser does Parser {
	method takes-argument(--> Bool) {
		return True;
	}
	method parse($raw, $others) {
		my $name = $!name;
		if $raw ~~ $!name {
			if $others.elems {
				return $others.shift;
			}
			else {
				die "Expected an argument";
			}
		}
		elsif $raw ~~ / ^ $name '=' $<value>=[.*] / -> $/ {
			return $<value>;
		}
		else {
			die "$raw can't match $name?";
		}
	}
}

class CountParser does Parser {
	method takes-argument(--> Bool) {
		return False;
	}
	method parse($raw, $others) {
	}
}

class MaybeParser does Parser {
	has $.default;
	method takes-argument(--> Bool) {
		return True;
	}
	method parse($raw, $others) {
		my $name = $!name;
		if $raw ~~ $!name {
			if $others.elems {
				return $others.shift;
			}
			else {
				return $!default;
			}
		}
		elsif $raw ~~ / ^ $name '=' $<value>=[.*] / -> $/ {
			return $<value>;
		}
		else {
			die "$raw can't match $name?";
		}
	}
}

role Converter {
	method convert() { ... }
}

class NullConverter does Converter {
	method convert(Any:D $value) {
		return $value;
	}
}

class IntConverter does Converter {
	method convert(Any:D $value) {
		return $value.Int;
	}
}

role Storer {
	method store($value) { ... }
}

role Gatherer {
	method storage-for($name) { ... }
}

class CaptureStorer does Storer {
	has Capture:D $.capture is required;
	method store($value) {
		|$!capture = $value;
	}
}

class CaptureGatherer does Gatherer {
	has Capture:D $.capture is required;
	method storage-for($name) {
		return CaptureStorer.new(:$!capture):
	}
}

class HashStorer does Storer {
	has Hash:D $.hash is required;
	has Str:D $.key is required;
	method store($value) {
		$!hash{$!key} = $value;
	}
}

class HashGatherer does Gatherer {
	has Hash:D $.hash is required;
	method storage-for($key) {
		return HashStorer.new(:$!hash, :$key);
	}
}

role Multiplexer {
	has Converter:D $.converter = NullConverter.new;
	method store($value) { ... }
}

class Monoplexer does Multiplexer {
	has Storer:D $.storer is required;
	method store($value) {
		$!storer.store($!converter.convert($value));
	}
}

class Arrayplexer does Multiplexer {
	has Array $.array = Array.new;
	method store(Any:D $value) {
		$!array.push($!converter.convert($value));
	}
}

class HashPlexer does Multiplexer {
	has Hash $.hash = Hash.new;
	method store(Any:D $pair) {
		my ($key, $value) = $pair.split('=', 2);
		$!hash{$key} = $!converter.convert($value);
	}
}

class Option {
	has Parser:D $.parser is required handles <name names takes-argument>;
	has Multiplexer:D $.multiplexer is required;
	method match(Str:D $raw, $args) {
		my $value = $!parser.parse($raw, $args);
		$!multiplexer.store($value);
	}
}

has Bool:D $.gnu-style = True;
has Bool:D $.permute = False;
enum Storage <StoreScalar StoreArray StoreHash>;

multi parse-option(Str $pattern where rx/ ^ $<name>=[\w+] $<negatable>=['!'?] $ /, Gatherer $gatherer) {
	my $storer = $gatherer.storage-for($<name>);
	return Option.new(
		parser => BooleanParser.new(:name($<name>), :negatable(?$<negatable>)),
		multiplexer => Monoplexer.new(:$storer, :converter(NullConverter.new)),
	);
}

multi storage-for('%', Storer :$storer, :$converter) {
}
multi storage-for('@', Storer :$storer, :$converter) {
	my $plexer = Arrayplexer.new();
	$storer.store($plexer.array);
	return $plexer;
}
multi storage-for(Any $foo, Storer :$storer, :$converter) {
	warn $foo.perl;
	return Monoplexer.new(:$storer, :$converter);
}
multi parse-option(Str $pattern where rx/ ^ $<name>=[\w+] '=' $<type>=<[si]> $<class>=[<[%@]>?] /, Gatherer $gatherer) {
	my $parser = ArgumentedParser.new(:name(~$<name>));
	my $storer = $gatherer.storage-for($parser.name);
	my $converter = $<type> && $<type> eq 'i' ?? IntConverter.new !! NullConverter.new;
	my $multiplexer = storage-for(~$<class>, :$storer, :$converter);
	return Option.new(:$parser, :$multiplexer);
}
multi parse-option(Str $pattern where rx/ ^ $<name>=[\w+] '+' $ /, Gatherer $gatherer) {

}

multi parse-option(Str $pattern, Gatherer $gatherer) {
	die "Invalid pattern '$pattern'";
}

multi method getopt(@args, *@patterns) {
	my (%options, %hash);
	for @patterns -> $pattern {
		my $storer = HashGatherer.new(:hash(%hash));
		my Option $option = parse-option($pattern, $storer);
		%options{$option.name} = $option;
	}
	my @list = self!getopt(@args, %options);
	return \(|@list, |%hash);
}
multi method getopt(@args, *%patterns) {
	my %options;
	for %patterns.kv -> $pattern, $capture {
		my $storer = CaptureGatherer.new(:$capture);
		my Option $option = parse-option($pattern, $storer);
		%options{$option.name} = $option;
	}
	self!getopt(@args, %options);
}

method !getopt(@argv, %options) {
	my @args = @argv;
	my @ret;
	while @args {
		my $head = @args.shift;
		if $head ~~ / ^ '--' $<name>=[\w+] $ / -> $/ {
			if %options{$<name>} -> $option {
				$option.match(~$<name>, @args);
			}
			else {
				die "Unknown option $<name>: " ~ %options.perl;
			}
		}
		elsif $!gnu-style && $head ~~ / ^ '--' $<value>=[ $<name>=[\w+] '=' .* ] / -> $/ {
			if %options{$<name>} -> $option {
				die "Option {$option.name} doesn't take arguments" unless $option.takes-argument;
				$option.match(~$<value>, @args);
			}
			else {
				die "Unknown option $<name>";
			}
		}
		else {
			if $!permute {
				@ret.push: $head;
			}
			else {
				@ret.append: $head, |@args;
				last;
			}
		}
	}
	return @ret;
}
