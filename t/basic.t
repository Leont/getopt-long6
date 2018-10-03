use v6;

use Test;

use Getopt::Long;

my $capture = get-options-from(<--foo bar --fooo bar2 -f bar3 -ac --bar baz>, 'foo|f|fooo=s@', 'bar', 'a', 'c');
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

my $capture6 = get-options-from([<--quz=2.5>], 'quz=f');
is-deeply($capture6, \(:quz(2.5)), 'Numeric arguments work');

my $capture7 = get-options-from(['--quz'], 'quz:i');
is-deeply($capture7, \(:quz(0)), ':i without argument works');

my $capture8 = get-options-from(<--quz 2>, 'quz:i');
is-deeply($capture8, \(:quz(2)), ':i with argument works');

my $capture9 = get-options-from(['--quz'], 'quz:1');
is-deeply($capture9, \(:quz(1)), ':1 without argument works');

my $capture10 = get-options-from(<--quz 2>, 'quz:1');
is-deeply($capture10, \(:quz(2)), ':1 with argument works');

my $capture11 = get-options-from(<--foo --foo>, 'foo+');
is-deeply($capture11, \(:foo(2)), 'Counter adds up');

my $capture12 = get-options-from(['--foo'], 'foo:+');
is-deeply($capture12, \(:foo(1)), 'Colon singles fine');

my $capture13 = get-options-from(<--foo 2 --foo>, 'foo:+');
is-deeply($capture13, \(:foo(3)), 'Colon counter adds up');

my $capture14 = get-options-from(<--bar 012>, 'bar=o');
is-deeply($capture14, \(:bar(10)), 'Parsing octal argument with "o"');

my $capture15 = get-options-from(<--bar -012>, 'bar=o');
is-deeply($capture15, \(:bar(-10)), 'Parsing negative octal argument with "o"');

my $capture16 = get-options-from(<--bar 12>, 'bar=o');
is-deeply($capture16, \(:bar(12)), 'Parsing decimal argument with "o"');

my $capture17 = get-options-from(['--no-bar'], 'bar!');
is-deeply($capture17, \(:!bar), 'Negated arguments produce False');

my $capture18 = get-options-from(['-abc'], <a b c abc>, :!bundling);
is-deeply($capture18, \(:abc), 'Bundling can be disabled');

my $capture19 = get-options-from(['--foo', '1', '2', '3'], <foo=i{2}>);
is-deeply($capture19, \(val('3'), :foo(Array[Int].new(1, 2))), 'Repeat specifier works');

my $capture20 = get-options-from(['--foo', '1', '2', '3'], <foo=i{1,2}>);
is-deeply($capture20, \(val('3'), :foo(Array[Int].new(1, 2))), 'Repeat specifier works with range');

my $getopt2 = Getopt::Long.new(sub (:$foo is getopt("=s%")) {});

my $capture21 = $getopt2.get-options(<--foo bar=buz --foo qaz=quz>);
my Str %expected = :bar('buz'), :qaz('quz');
is-deeply($capture21, \(:foo(%expected)), 'getopt trait works');

my $getopt3 = Getopt::Long.new(sub (Bool :$foo = True) { });

my $capture22 = $getopt3.get-options(['--no-foo']);
is-deeply($capture22, \(:foo(False)), 'negative argument detected');

done-testing;
