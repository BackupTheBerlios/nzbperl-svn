#!/usr/bin/perl
#

use strict;
use threads;
use Thread::Queue;

my $threadQueue = Thread::Queue->new;
sub readerThread {
	print "Thread started\n";
	while(1){
		my $e = $threadQueue->dequeue;
		print "Thread message: $e\n";
		$e =~ /quit/ and last;
	}
	print "Thread exiting\n";
}

my $thread = threads->new(\&readerThread);

while(<>){
	chomp;
	$threadQueue->enqueue($_);
	if($_ =~ /quit/){
		last;
	}
}

$thread->join;

