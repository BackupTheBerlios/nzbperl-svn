#!/usr/bin/perl

use lib '..';
#print "\@INC is @INC\n";

package SegmentTest;
use base qw(Test::Unit::TestCase);
use strict;
use nzbperl;

use Test::Unit::TestRunner;
my $testrunner = Test::Unit::TestRunner->new();
$testrunner->start('SegmentTest');

sub new {
	my $self = shift()->SUPER::new(@_);
	return $self;
}

sub test_constructor {
	my $self = shift;
	my $seg = Nzb::Segment->new('myid', 1234, 12);	#msgid, size, number
	$self->assert_equals(12, $seg->number, 'Number should be 12');
	$self->assert_equals(1234, $seg->size, 'Size should be 1234');
	$self->assert_equals('myid', $seg->id, 'Wrong msgid');
}

sub test_parentFile {
	my $self = shift;
	my $seg = Nzb::Segment->new('myid', 1234, 12);	#msgid, size, number
	$self->assert_null($seg->parentFile);
	my $file = Nzb::File->new('myfile.rar', 'date', undef, undef);
	$seg->parentFile($file);
	$self->assert_not_null($seg->parentFile);
	$self->assert_equals($file, $seg->parentFile, 'Wrong file!?');
}

sub test_markAsComplete {
	my $self = shift;
	my $seg = Nzb::Segment->new('myid', 1234, 12);	#msgid, size, number
	my $file = Nzb::File->new('myfile.rar', 'date', undef, undef);

	$seg->markAsComplete;	# Verify that it doesn't assert, even w/out a parent

	$seg->parentFile($file);
	$self->assert_equals(0, $file->completedSegmentCount);
	$seg->markAsComplete;
	$self->assert_equals(1, $file->completedSegmentCount);
}
