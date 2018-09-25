use v6;

use Test;

use Getopt::Long;

my $capture = Getopt::Long.new('foo|f|fooo=s@', 'bar', 'a', 'c').get-options(<--foo bar --fooo bar2 -f bar3 -ac --bar baz>);
is-deeply($capture, \('baz', :bar, :a, :c, :foo(Array[Str].new(<bar bar2 bar3>))), 'Common argument mix works');

multi main(*@, Str :fooo(:f(:@foo)), Bool :$bar) {
}
multi main(*@, Bool :$a!, Bool :$c!, Bool :$d) {
}
my $getopt = Getopt::Long.new(&main);

my $capture2 = $getopt.get-options(<--foo bar --fooo bar2 --bar baz>);
is-deeply($capture2, \('baz', :bar, :foo(Array[Str].new(<bar bar2>))), 'Common argument mix works (2)');
lives-ok( { main(|$capture2) }, 'Calling main (1) works');

my $capture3 = $getopt.get-options(<-ac -dfbar3>);
is-deeply($capture3, \(:a, :c, :d, :foo(Array[Str].new(<bar3>))), 'Short options work');

my $capture4 = $getopt.get-options(<--foo bar --fooo bar2 -f bar3 -ac --bar baz>);
dies-ok( { main(|$capture4) }, 'Calling main (1) works');

my $capture5 = $getopt.get-options(<--bar -- -a>);
is-deeply($capture5, \('-a', :bar), '"--" terminates argument handling');

my $capture6 = Getopt::Long.new('quz=f').get-options([<--quz=2.5>]);
is-deeply($capture6, \(:quz(2.5)), 'Numeric arguments work');

my $getopt2 = Getopt::Long.new('quz:i');

my $capture7 = $getopt2.get-options(['--quz']);
is-deeply($capture7, \(:quz(0)), ':i without argument works');

my $capture8 = $getopt2.get-options(<--quz 2>);
is-deeply($capture8, \(:quz(2)), ':i with argument works');

my $getopt3 = Getopt::Long.new('quz:1');

my $capture9 = $getopt3.get-options(['--quz']);
is-deeply($capture9, \(:quz(1)), ':1 without argument works');

my $capture10 = $getopt3.get-options(<--quz 2>);
is-deeply($capture10, \(:quz(2)), ':1 with argument works');

my $getopt4 = Getopt::Long.new('foo+');

my $capture11 = $getopt4.get-options(<--foo --foo>);
is-deeply($capture11, \(:foo(2)), 'Counter adds up');

my $getopt5 = Getopt::Long.new('foo:+');

my $capture12 = $getopt5.get-options(['--foo']);
is-deeply($capture12, \(:foo(1)), 'Colon singles fine');

my $capture13 = $getopt5.get-options(<--foo 2 --foo>);
is-deeply($capture13, \(:foo(3)), 'Colon counter adds up');

my $getopt6 = Getopt::Long.new('bar=o');

my $capture14 = $getopt6.get-options(<--bar 012>);
is-deeply($capture14, \(:bar(10)), 'Parsing octal argument with "o"');

my $capture15 = $getopt6.get-options(<--bar -012>);
is-deeply($capture15, \(:bar(-10)), 'Parsing negative octal argument with "o"');

my $capture16 = $getopt6.get-options(<--bar 12>);
is-deeply($capture16, \(:bar(12)), 'Parsing decimal argument with "o"');

my $getopt7 = Getopt::Long.new('bar!');

my $capture17 = $getopt7.get-options(['--no-bar']);
is-deeply($capture17, \(:!bar), 'Negated arguments produce False');

my $getopt8 = Getopt::Long.new(<a b c abc>, :!bundling);

my $capture18 = $getopt8.get-options(['-abc']);
is-deeply($capture18, \(:abc), 'Bundling can be disabled');

my $getopt9 = Getopt::Long.new(<foo=i{2}>);

my $capture19 = $getopt9.get-options(['--foo', '1', '2', '3']);
is-deeply($capture19, \(val('3'), :foo(Array[Int].new(1, 2))), 'Repeat specifier works');

my $getopt10 = Getopt::Long.new(<foo=i{1,2}>);

my $capture20 = $getopt10.get-options(['--foo', '1', '2', '3']);
is-deeply($capture20, \(val('3'), :foo(Array[Int].new(1, 2))), 'Repeat specifier works with range');

my $getopt11 = Getopt::Long.new(sub (:$foo is getopt("=s%")) {});

my $capture21 = $getopt11.get-options(<--foo bar=buz --foo qaz=quz>);
my Str %expected = :bar('buz'), :qaz('quz');
is-deeply($capture21, \(:foo(%expected)), 'getopt trait works');

my $getopt12 = Getopt::Long.new(sub (Bool :$foo = True) { });

my $capture22 = $getopt12.get-options(['--no-foo']);
is-deeply($capture22, \(:foo(False)), 'negative argument detected');

done-testing;
