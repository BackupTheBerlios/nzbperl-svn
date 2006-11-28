#!/usr/bin/perl

use lib '..';

package ConfigTest;
use base qw(Test::Unit::TestCase);
use strict;
use nzbperl;

use Test::Unit::TestRunner;
my $testrunner = Test::Unit::TestRunner->new();
$testrunner->start('ConfigTest');

sub new {
	my $self = shift()->SUPER::new(@_);
	return $self;
}

sub test_constructor {
	my $self = shift;
	my $config = Nzb::Config->new();
	# Check a default value
	$self->assert_not_null($config->configfile);
	$self->assert_equals('', $config->user);	# default
}

sub test_confirm_proxy {
	my $self = shift;
	my $config = Nzb::Config->new();
	$config->port(123);
	$self->assert_equals(123, $config->port);
	#assert_$config->doesnotexist;
}

sub test_hostport {
	my $self = shift;
	my ($host, $port) = Nzb::Config->hostport("justaserver");
	$self->assert_equals('justaserver', $host);
	$self->assert_equals(-1, $port);
	($host, $port) = Nzb::Config->hostport("srv:81");
	$self->assert_equals('srv', $host);
	$self->assert_equals(81, $port);
}

sub test_getDestDirForFile {
	my $self = shift;
	my $config = Nzb::Config->new();
	my @segments = (
		Nzb::Segment->new('myid1', 2, 12), 
		Nzb::Segment->new('myid2', 3, 12)
	);
	my $file = Nzb::File->new('myfile.rar', 'date', ['alt.foo.grp', 'not.used'], \@segments);

	my $nzb = Nzb::NzbFile->new('/nzbpath/parent.Nzb', ($file));

	my $dir = $config->getDestDirForFile($file);
	$self->assert_null($dir);

	# Basic dlpath config option
	$config->dlpath('/path/to/dl');
	$dir = $config->getDestDirForFile($file);
	$self->assert_equals('/path/to/dl', $dir);

	# dlcreate config option - create subdir based on parent nzb filename
	$config->dlcreate(1);
	$dir = $config->getDestDirForFile($file);
	$self->assert_equals('/path/to/dl/parent', $dir);
	
	# dlcreategrp config option
	$config->dlcreategrp(1);
	$config->dlcreate(undef);	# unset dlcreate -- mutually exclusive
	$dir = $config->getDestDirForFile($file);
	$self->assert_equals('/path/to/dl/alt.foo.grp', $dir);

	$config->dlrelative(1);
	$config->dlpath(undef);
	$dir = $config->getDestDirForFile($file);
	$self->assert_equals('/nzbpath', $dir);

}
