use v6;

use Test;

use Getopt::Long;

my $capture = Getopt::Long.new('foo|f|fooo=s@', 'bar', 'a', 'c').get-options(<--foo bar --fooo bar2 -f bar3 -ac --bar baz>);
is-deeply($capture, \('baz', :bar, :a, :c, :foo(Array[Str].new(<bar bar2 bar3>))), 'Common argument mix works');

multi main(*@, Str :fooo(:f(:@foo)), Bool :$bar) {
}
multi main(*@, Bool :$a!, Bool :$c!) {
}
my $getopt = Getopt::Long.new(&main);

my $capture2 = $getopt.get-options(<--foo bar --fooo bar2 --bar baz>);
is-deeply($capture2, \('baz', :bar, :foo(Array[Str].new(<bar bar2>))), 'Common argument mix works (2)');
lives-ok( { main(|$capture2) }, 'Calling main (1) works');

my $capture3 = $getopt.get-options(<-ac -fbar3>);
is-deeply($capture3, \(:a, :c, :foo(Array[Str].new(<bar3>))), 'Short options work');

my $capture4 = $getopt.get-options(<--foo bar --fooo bar2 -f bar3 -ac --bar baz>);
dies-ok( { main(|$capture4) }, 'Calling main (1) works');

my $capture5 = $getopt.get-options(<--bar -- -a>);
is-deeply($capture5, \('-a', :bar), '"--" terminates argument handling');

done-testing;
