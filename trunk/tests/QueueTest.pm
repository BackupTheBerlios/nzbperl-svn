#!/usr/bin/perl

use lib '..';

package QueueTest;
use base qw(Test::Unit::TestCase);
use strict;
use nzbperl;

use Test::Unit::TestRunner;
my $testrunner = Test::Unit::TestRunner->new();
$testrunner->start('FileTest');

sub new {
	my $self = shift()->SUPER::new(@_);
	return $self;
}

sub test_constructor {
	my $self = shift;
	my $q = Nzb::Queue->new();
	$self->assert($q->isEmpty());
	$self->assert_equals(0, $q->pendingFileCount());
}

sub test_queueNzb {
	my $self = shift;

	my $q = Nzb::Queue->new();
	my $nzbFile = undef;
	my $rc = $q->queueNzb($nzbFile);

	$self->assert_equals(0, $rc);
	$self->assert($q->isEmpty());

	$nzbFile = $self->_buildSimpleNzb('nzb1.nzb');;

	$rc = $q->queueNzb($nzbFile);
	$self->assert(not $q->isEmpty());
	$self->assert_equals(1, $rc);
	$self->assert_equals(2, $q->pendingFileCount);
	$self->assert_equals(0, $q->pendingNzbFileCount);

	$nzbFile = $self->_buildSimpleNzb('nzb2.nzb');;
	$rc = $q->queueNzb($nzbFile);
	$self->assert_equals(2, $rc);
	$self->assert_equals(4, $q->pendingFileCount);
	$self->assert_equals(0, $q->pendingNzbFileCount);
}

sub test_pendingNzbFileCount {
	my $self = shift;

	my $q = Nzb::Queue->new();
	$self->assert_equals(0, $q->pendingNzbFileCount);	# starts empty/none

	$q->queueNzb($self->_buildSimpleNzb);
	$self->assert_equals(0, $q->pendingNzbFileCount);

	$q->addNewFilename('fubar.nzb');
	$self->assert_equals(1, $q->pendingNzbFileCount);
	$q->addNewFilename('fubar.nzb');
	$self->assert_equals(1, $q->pendingNzbFileCount);	# seen, so not bumped
	$q->addNewFilename('newbar.nzb');
	$self->assert_equals(2, $q->pendingNzbFileCount);	# not seen, so bumped

}

sub test_haveSeen {
	my $self = shift;
	my $q = Nzb::Queue->new();
	$self->assert(not $q->haveSeen('neverseen.nzb'));
	$q->queueNzb($self->_buildSimpleNzb);
	$self->assert($q->haveSeen('nzbfile.nzb'));
	$self->assert($q->haveSeen('/any/path/to/nzbfile.nzb'));
	$q->addNewFilename('/path1/to/my.nzb');
	$self->assert($q->haveSeen('/any/path/to/my.nzb'));
}

sub test_addNewFilename {
	my $self = shift;
	my $q = Nzb::Queue->new();
	$self->assert_equals(1, $q->addNewFilename('first.nzb'));
	$self->assert_equals(1, $q->addNewFilename('first.nzb'));
	$self->assert_equals(2, $q->addNewFilename('second.nzb'));
}

sub test_getNextSegment {
	my $self = shift;
	my $q = Nzb::Queue->new();
	$q->queueNzb($self->_buildSimpleNzb);
	my $seg = $q->getNextSegment();
	$self->assert_not_null($seg);
	$self->assert_equals(1, $seg->number);
	$self->assert_equals(2, $q->getNextSegment()->number());
	$self->assert_equals(3, $q->getNextSegment()->number());
	$self->assert_equals(4, $q->getNextSegment()->number());

	$self->assert_null($q->getNextSegment());
}

sub test_getNextNzb {
	my $self = shift;
	my $q = Nzb::Queue->new();
	$self->assert(not defined($q->getNextNzb()));	# nothing pending
	$q->addNewFilename('/path/to/fonzie.nzb');
	$self->assert_equals('/path/to/fonzie.nzb', $q->getNextNzb());
}

sub test_dequeueSegPullsFromPending {
	my $self = shift;
	my $q = Nzb::Queue->new();

	$q->queueNzb($self->_buildSimpleNzb('1.nzb'));
	$q->queueNzb($self->_buildSimpleNzb('2.nzb'));
	$self->assert_equals(2, $q->pendingNzbFileCount);

	foreach (0..4){
		print "DEBUG:\n";
		$q->getNextSegment();
	}
	$self->assert_equals(0, $q->pendingNzbFileCount);
}

# Create nzb file with 2 files inside, each with 2 segments
sub _buildSimpleNzb {
	my ($self, $nzbfn) = @_;
	$nzbfn = 'nzbfile.nzb' unless defined $nzbfn;

	my $f1 = Nzb::File->new('myfile1.rar', 'date', 
		['alt.foo.grp', 'not.used'], 
		[ Nzb::Segment->new('myid1', 256, 1), Nzb::Segment->new('myid2', 256, 2)]);

	my $f2 = Nzb::File->new('myfile2.rar', 'date', 
		['alt.foo.grp', 'not.used'], 
		[ Nzb::Segment->new('myid3', 256, 3), Nzb::Segment->new('myid4', 256, 4)]);

	return Nzb::NzbFile->new($nzbfn, ($f1, $f2));
}

sub test_computeTotalNzbSize {
	my $self = shift;
	my $q = Nzb::Queue->new();
	$self->assert_equals(0, $q->computeTotalNzbSize());

	$q->queueNzb($self->_buildSimpleNzb);
	$self->assert_equals(4*256, $q->computeTotalNzbSize());
}

sub test_isEmpty {
	my $self = shift;
	my $q = Nzb::Queue->new();
	$self->assert($q->isEmpty());

	my $nzbFile = undef;
	$q->queueNzb($nzbFile);
	$self->assert($q->isEmpty());

	$nzbFile = Nzb::NzbFile->new('file.nzb', ());
	$q->queueNzb($nzbFile);
	$self->assert(not $q->isEmpty());
}

