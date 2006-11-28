#!/usr/bin/perl -w -T

use LWP::Simple;
my $UPDATE_URL = 'http://noisybox.net/perltest.txtx';
my $remote_doc = eval "get \"$UPDATE_URL\"";
print "remote doc is $remote_doc";
