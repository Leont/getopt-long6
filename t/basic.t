use v6;

use Test;

use Getopt::Long;

my $capture = get-options-from(<--foo bar --fooo bar2 -f bar3 -ac --bar baz>, 'foo|f|fooo=s@', 'bar', 'a', 'c');
is-deeply($capture, \('baz', :bar, :a, :c, :foo(Array[Str].new(<bar bar2 bar3>))), 'Common argument mix works');

multi main(*@, Str :fooo(:f(:@foo)), Bool :$bar) is getopt {
}
multi main(*@, Bool :$a!, Bool :$c!, Bool :$d) {
}
my $getopt = Getopt::Long.new-from-sub(&main);

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
is-deeply($capture6, \(:quz(2.5e0)), 'Floating point arguments work');

my $capture7 = get-options-from(['--quz'], 'quz:i');
is-deeply($capture7, \(:0quz), ':i without argument works');

my $capture8 = get-options-from(<--quz 2>, 'quz:i');
is-deeply($capture8, \(:2quz), ':i with argument works');

my $capture9 = get-options-from(['--quz'], 'quz:1');
is-deeply($capture9, \(:1quz), ':1 without argument works');

my $capture10 = get-options-from(<--quz 2>, 'quz:1');
is-deeply($capture10, \(:2quz), ':1 with argument works');

my $capture11 = get-options-from(<--foo --foo>, 'foo+');
is-deeply($capture11, \(:2foo), 'Counter adds up');

my $capture12 = get-options-from(['--foo'], 'foo:+');
is-deeply($capture12, \(:1foo), 'Colon singles fine');

my $capture13 = get-options-from(<--foo 2 --foo>, 'foo:+');
is-deeply($capture13, \(:3foo), 'Colon counter adds up');

my $capture14 = get-options-from(<--bar 0o12>, 'bar=i');
is-deeply($capture14, \(:10bar), 'Parsing octal argument with "i"');

my $capture15 = get-options-from(<--bar -0o12>, 'bar=i');
is-deeply($capture15, \(:bar(-10)), 'Parsing negative octal argument with "i"');

my $capture16 = get-options-from(<--bar 12>, 'bar=i');
is-deeply($capture16, \(:12bar), 'Parsing decimal argument with "i"');

my $capture17 = get-options-from(['--no-bar'], 'bar!');
is-deeply($capture17, \(:!bar), 'Negated arguments produce False');

my $capture18 = get-options-from(['-abc'], <a b c abc>, :!bundling);
is-deeply($capture18, \(:abc), 'Bundling can be disabled');

my $capture19 = get-options-from(['--foo', '1', '2', '3'], <foo=i{2}>);
is-deeply($capture19, \('3', :foo(Array[Int].new(1, 2))), 'Repeat specifier works');

my $capture20 = get-options-from(['--foo', '1', '2', '3'], <foo=i{1,2}>);
is-deeply($capture20, \('3', :foo(Array[Int].new(1, 2))), 'Repeat specifier works with range');

my $getopt2 = Getopt::Long.new-from-sub(sub (:$foo is getopt("=s%")) is getopt {});

my $capture21 = $getopt2.get-options(<--foo bar=buz --foo qaz=quz>);
my Str %expected = :bar('buz'), :qaz('quz');
is-deeply($capture21, \(:foo(%expected)), 'getopt trait works');

my $getopt3 = Getopt::Long.new-from-sub(sub (Bool :$foo = True) { });

my $capture22 = $getopt3.get-options(['--no-foo']);
is-deeply($capture22, \(:!foo), 'negative argument detected');

get-options-from([<--foo>], 'foo' => my $foo);
is-deeply($foo, True, 'Pair arguments');

get-options-from(<--foo 1 --foo 2>, 'foo=i@' => my @foo);
is-deeply(@foo, [1, 2], 'Pair arguments');

my $capture23 = get-options-from(['--foo', '1+2i'], <foo=c>);
is-deeply($capture23, \(:foo(1+2i)), 'Repeat specifier works');

my $capture24 = get-options-from(['-f=1'], <f=i>, :compat-singles);
is-deeply($capture24, \(:f(1)), ':compat-singles appears to work');

my $capture25 = get-options-from(['-/f'], <f!>, :compat-negation);
is-deeply($capture25, \(:!f), ':compat-negation works');

my $capture26 = get-options-from(['--/f=foo'], <f=s>, :compat-negation);
ok(!$capture26<f>, 'compat negation delivers a false value');
is($capture26<f>, 'foo', 'compat negation delivers the correct string');

my $getopt4 = Getopt::Long.new-from-sub(sub (Order :$order) { });
my $capture27 = $getopt4.get-options(<--order Same>);
is($capture27, \(:order(Same)), 'Correctly parsed enum');

my $getopt5 = Getopt::Long.new-from-sub(sub (Int $foo, Rat $bar) { });
my $capture28 = $getopt5.get-options(["1", "2.5"]);
is($capture28, \(1, 2.5), 'Typed positionals work');

multi main2(Int $foo) { }
multi main2(Int $foo, Rat $bar) { }
my $getopt6 = Getopt::Long.new-from-sub(&main2);
my $capture29 = $getopt5.get-options(["1", "2.5"]);
is($capture29, \(1, 2.5), 'Typed positionals work on multis as well');

my $getopt7 = Getopt::Long.new-from-sub(sub (DateTime :$datetime) { });
my $capture30 = $getopt7.get-options(<--datetime 2015-11-21T08:01:00+0100 >);
is($capture30, \(:datetime(DateTime.new(:2015year, :11month, :21day, :8hour, :1minute, :3600timezone))), 'Can parse DateTime');

my $capture31 = get-options-from(<--date 2015-11-21>, <date=a>);
is-deeply($capture31, \(:date(Date.new(:2015year, :11month, :21day))), 'Can parse Date');

done-testing;
