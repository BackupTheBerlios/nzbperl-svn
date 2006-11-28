#!/usr/bin/perl

package NzbperlTestSuite;

use base qw(Test::Unit::TestSuite);

sub name { 'nzbperl test suite'}
sub include_tests { qw(SegmentTest FileTest ConfigTest QueueTest) }

use Test::Unit::TestRunner;
my $testrunner = Test::Unit::TestRunner->new();
$testrunner->start('NzbperlTestSuite');
