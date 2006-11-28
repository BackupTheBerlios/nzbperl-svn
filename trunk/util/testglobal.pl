
package GT;
use strict;

my %foo;
$foo{'bar'} = 'something';
$foo{'yes'} = 1;

my $test = GT::MySub->new();

printf("I think bar is %s\n", $test->getBar());


package GT::MySub;
sub new {
	my $class = shift;
	my $self = {};
	bless($self, $class);
	return $self;
}

sub getBar {
	my $self = shift;
	return $foo{'bar'};
}
