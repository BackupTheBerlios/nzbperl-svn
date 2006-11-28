#!/usr/bin/perl

use lib '..';

package FileTest;
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
	my $file = Nzb::File->new('myfile.rar', 'date', undef, undef);	#name, date, groups, segments
	$self->assert_not_null($file->uid);
	$self->assert_equals(0, $file->completedSegmentCount);
	$self->assert_null($file->parentNzb);
	$self->assert_null($file->trueFilename);
}

sub test_constructor_sets_parentFile {
	my $self = shift;
	my $seg = Nzb::Segment->new('myid', 1234, 12); 
	my $file = Nzb::File->new('myfile.rar', 'date', undef, [$seg]);	#name, date, groups, segments
	$self->assert_equals($file, $seg->parentFile);
}

sub test_instances_with_different_uids {
	my $self = shift;
	my $file1 = Nzb::File->new('myfile1.rar', 'date', undef, undef);	#name, date, groups, segments
	my $file2 = Nzb::File->new('myfile2.rar', 'date', undef, undef);	#name, date, groups, segments

	$self->assert_not_null($file1->uid);
	$self->assert_not_null($file2->uid);
	$self->assert_not_equals($file1->uid, $file2->uid);
}

sub test_segmentCount {
	my $self = shift;
	my $file = $self->_buildFileWithTwoSeg;
	$self->assert_equals(2, $file->segmentCount, 'Wrong count');
}

sub test_parentNzb {
	my $self = shift;
	my $file = Nzb::File->new('myfile.rar', 'date', undef, undef);	#name, date, groups, segments
	$self->assert_null($file->parentNzb);
	my $parentNzb = Nzb::NzbFile->new('file.nzb', ($file));
	$file->parentNzb($parentNzb);
	$self->assert_equals($parentNzb, $file->parentNzb);
}

sub test_percentComplete {
	my $self = shift;
	my $file = $self->_buildFileWithTwoSeg;
	my @seg = $file->segments();
	$self->assert_equals(0, $file->percentComplete);	# 
	$file->addDownloadedBytes(2);
	$self->assert_equals(40, $file->percentComplete);
	$file->addDownloadedBytes(3);
	$self->assert_equals(100, $file->percentComplete);	# upper limit
	$file->addDownloadedBytes(3);			# Go over upper limit
	$self->assert_equals(100, $file->percentComplete);	# Capped at 100
	$file = Nzb::File->new('myfile.rar', 'date', undef, undef);	#name, date, groups, segments
	$self->assert_equals(0, $file->percentComplete);	# divide by zero guarded
}

sub test_totalSize {
	my $self = shift;
	my $file = $self->_buildFileWithTwoSeg;
	$self->assert_equals(5, $file->totalSize);
}

sub _buildFileWithTwoSeg {
	my $self = shift;
	my @segments = (
		Nzb::Segment->new('myid1', 2, 12), 
		Nzb::Segment->new('myid2', 3, 12)
	);
	return Nzb::File->new('myfile.rar', 'date', undef, \@segments);	#name, date, groups, segments
}
