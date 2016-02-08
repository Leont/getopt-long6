use v6;

use Test;

use Getopt::Long;

my $capture = Getopt::Long.new('foo|fooo=s@', 'bar').get-options(<--foo bar --fooo bar2 --bar baz>);

is-deeply($capture, \('baz', :bar, :foo(Array[Str].new(<bar bar2>))), '');

sub main(*@, Str :fooo(:@foo), Bool :$bar) {
}
my $capture2 = Getopt::Long.new(&main).get-options(<--foo bar --fooo bar2 --bar baz>);

is-deeply($capture2, \('baz', :bar, :foo(Array[Str].new(<bar bar2>))), '');

lives-ok( { main(|$capture2) } );

done-testing;
