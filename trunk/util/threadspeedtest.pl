#!/usr/bin/perl
#


use strict;
use threads;
BEGIN {
use Net::NNTP;
use XML::DOM;
use Data::Dumper;
use Getopt::Long;
use Time::HiRes;    # timer stuff
use Thread::Queue;
use Term::ReadKey;  # for no echo password reading

}

my @threads;
foreach my $i (1..10){
	$threads[$i] = threads->new(\&worker, $i);
}
print "All threads created.\n";

foreach my $i (1..10){
	$threads[$i]->join();
}

sub worker {
	my $id = shift;
	print "worker id = $id\n";
	Net::NNTP->new('localhost', 'Debug' => 0);

}


