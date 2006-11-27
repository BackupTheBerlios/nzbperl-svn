#!/usr/bin/perl
#
# nzbperl.pl -- version 0.03
# 
# for more information:
# http://noisybox.net/computers/nzbperl/ 
#
#########################################################################################
# Copyright (C) 2004 jason plumb
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#########################################################################################
#
# General TODOs:
#   * Continue from the middle of an nzb, don't redownload if already on disk. (not sure how to tell)
#     Saving state between "sessions".
#   * Consider using .rc file to store parameters, commandline will override
#   * News server is currently required.  If server not given, we should fall back on 
#     environment var for NNTPSERVER
#   * Consider putting tempfile (.parts files) in /tmp or other customize location
#   * User specified output directory.
#   * Implement 's'kip key command (to skip current file)
#   * Implement 'q'uit key command (end program gracefully)
#   * Display line should have an ETA section
#   * Consider making the display line into 2 lines for formatting on smaller terms
#   * Runtime keys to incrementally bump up/down speed.
#   * Possibly doing regex filtering on filenames in nzb file?
#   * People want to have multiple connections to the server.
#

use strict;
use Net::NNTP;
use XML::DOM;
use Data::Dumper;
use Getopt::Long;
use Time::HiRes;	# timer stuff
use Term::ReadKey;	# for no echo password reading

# There's a note below that explains why we aren't using chunking anymore.
#my $chunksize = 3*1024;		# Chunk size when reading...pulled straight outta my ass.
my $dispchunkct = 250;			# Number of data lines to read between screen updates.
my $targkBps = 0;
my ($medbw, $lowbw) = (95, 35);	# Yeah, we only support med/low for now.

# Make stdout not buffered.
my $old_fh = select(STDOUT);
$| = 1;
select($old_fh);

my $quitnow = 0;
my $skipthisfile = 0;
my ($host, $user, $pw, $keep, $help, $nosort, $overwritefiles) =
	('', '', '', 0, 0, 0, 0);
GetOptions( 'server=s' => \$host, 'user=s' => \$user, 'pw=s' => \$pw, 'help' => \$help,
			'med=s' => \$medbw, 'low=s' => \$lowbw, 'speed=s' => \$targkBps, 
			'keep' => \$keep, 'nosort' => \$nosort, 'redo' => \$overwritefiles);
if($help){
	showUsage();
	exit;
}
if($keep){
	$keep = '';
}
else {
	$keep = '-c';
}


if($user and !$pw){
	print "Password for '$user': ";
	$pw = readPassword();
}
if(not $ARGV[0]){		# No NZB file given?
	showUsage(); 
	exit;
}

if(not length($host)){
	$host = $ENV{'NNTPSERVER'};
	not $host and die "Must provide --server or set \$NNTPSERVER environment\n";
}

my $nzbfilename = $ARGV[0];
my @fileset = parseNZB($nzbfilename, !$nosort);

#print Dumper(\@fileset);
#die;

my $nntp = Net::NNTP->new($host, 'Debug' => 0);
if($user){
	print "Authenticating to server...";
	my $rc = $nntp->authinfo($user, $pw);
	if(!$rc){
		print ">FAILED<\n";
		print "Please check the user/pass info and try again.\n";
		$nntp->quit;
		return;
	}
	print "success.\n";
}

my $lastgroup = '';
my ($wchar, $hchar, $wpixels, $hpixels) = GetTerminalSize();
my $displine = " " x ($wchar-1);   # This is cool -- it duplicates the string times itself.
my $sleepdur = 0;
my %bwinfo;

foreach my $file (@fileset){
	# todo: this group change thing may be busted.
	if(not ($lastgroup == $file->{'groups'}->[0])){
		$nntp->group($file->{'groups'}->[0]);
		$lastgroup = $file->{'groups'}->[0];
	}

	my $filesize = $file->{'totalsize'};
	my @filestarttime = Time::HiRes::gettimeofday();
	@{$bwinfo{'start'}} = Time::HiRes::gettimeofday();
	$bwinfo{'bytes'} = 0;

	print ("-" x $wchar) . "\n";
	print "Fetching " . $file->{'name'} . "...\n";
	print $displine;

	my $tmpfile = 'nzbperl.tmp' . time . '.parts';

	open OUT, '>' . $tmpfile or (print "Whoops!  Couldn't open temp parts file" and next);
	binmode OUT;
	my ($segsize, $partnum) = (0, 0);
	my $filebytesread = 0;
	my $dispchnk = 0;
	my $havefile = undef;
	$skipthisfile = 0;

	foreach my $segment (@{$file->{'segments'}}){
		($wchar, $hchar, $wpixels, $hpixels) = GetTerminalSize();

		$segsize = $segment->{'size'};
		my $msgid = $segment->{'msgid'};
		$partnum++;

		my $fh = $nntp->bodyfh('<' . $msgid . '>');

		my $segbytesread = 0;
		my $chunk;
		while(1){

			$chunk = <$fh>; 
			last unless $chunk;

			# Ok, this requires a huge note...apparently I'm the ONLY person who has ever tried
			# to use the tied filehandles with Net::NNTP.  So Net::NNTP uses Net::Cmd, which has
			# some weird behavior in the reading method...including this thing that replaces 
			# double dots at the start of lines with single dots.  Whatever...it's fucked, but
			# I guess it's there for a reason (trust me...it's needed).  So without this
			# replace, you get mangled body data.  This throws a wrench in the chunking approach,
			# so we're now line based, which sucks.  I suppose I could back buffer a few bytes, 
			# but I'm being lazy.  Yeah, back buffering is probably the way to do this *right*.
			$chunk =~ s/^\.\././o;

			if( !defined($havefile) and ($partnum == 1)){
				$havefile = fileExistsChecking($chunk);
			}

			print OUT $chunk;
			my $readrc = length($chunk);
			
			$segbytesread += $readrc;
			$filebytesread += $readrc;
			$bwinfo{'bytes'} += $readrc;

			$dispchnk++;
			if($dispchnk % $dispchunkct == 0){
				print "\b" x length($displine);

				$displine = buildDispLine($partnum, scalar @{$file->{'segments'}}, $segbytesread, 
											$segsize, $filebytesread, $filesize, \@filestarttime);
				print $displine;
				$dispchnk = 0;
			}

			checkAndDoKeyProcessing();
			if($quitnow){
				print "Forcing quit...shutting down.\n";
				$nntp->quit;
				close OUT;
				unlink $tmpfile;
				exit 0;
			}

			doThrottling();
			
		}# finished reading body

		if($havefile){
			if(!$overwritefiles){
				print "\nFile already exists on disk...skipping download.\n";
				$skipthisfile = 1;
				close OUT;
				unlink $tmpfile;
				last;
			}
		}
		if($skipthisfile){
			print "\nSkipping this file....\n";
			close OUT;
			unlink $tmpfile;
			last;
		}

		print "\b" x length($displine);
		$displine = buildDispLine($partnum, scalar @{$file->{'segments'}}, $segbytesread, 
									$segsize, $filebytesread, $filesize, \@filestarttime);
		print $displine;

	}

	next if $skipthisfile;

	# Ok, something is very strage here...I can't seem to get the parts and/or file sums
	# to match what's in the nzb file.  I suspect that the values in the nzb file are wrong...
	# No problems decoding tho, so for now I just hack this to make it look pretty....but
	# it's obviously still and st00pid and the numbers *should* be right.
	print "\b" x length($displine);
	$displine = buildDispLine($partnum, scalar @{$file->{'segments'}}, $segsize, 
								$segsize, $filesize, $filesize, \@filestarttime);
	print $displine;
	print "\n";

	close OUT;
	print "Decoding " . $file->{'name'};

	# Lots of assumptions here...like uudecode supporting yenc etc.
	`uudeview -i -a $keep -q "$tmpfile"`;
	print " -- finished\n";

}

$nntp->quit;



#########################################################################################
# Determines if we already have a file as determined by the chunk.  We assume here that
# the line *may* contain yenc or uuencoded file header information.  If it doesn't then
# we return undef, otherwise, we return 1 if the file exists or 0 if it does not.
#########################################################################################
sub fileExistsChecking {
	my $chunk = shift;
	chomp $chunk;
	if($chunk =~ /^=ybegin/){			# Yenc candidate
		# I'm assuming that the last tag on the line is "name=...", which I honestly have no idea
		# if that's always true.  :)
		$chunk =~ s/.* name=//;
	}
	elsif($chunk =~ /^begin \d+ /){		# UUencoded candidate
		$chunk =~ s/^begin \d+ //;
	}
	else{
		return undef;
	}

	opendir(DIR, ".") or die "Error checking output path for file existence: $!";
	my @match = grep(/^$chunk$/, readdir(DIR));
	closedir(DIR);
	if(scalar(@match) > 0){			# We have the file
		return 1;
	}
	return 0;
}

#########################################################################################
# Recomputes stuff needed for throttling etc.  TODO: Make this NOT rely on globals.
#########################################################################################
sub doThrottling {
	my $bytes = $bwinfo{'bytes'};
	my $start = $bwinfo{'start'};
	my $curbps = ($bytes/1024.0)/Time::HiRes::tv_interval($start);
	$bwinfo{'curbps'} = $curbps;
	if($targkBps){
		if($curbps > $targkBps){		# We're going too fast...
			if($sleepdur == 0){
				$sleepdur = 0.001;		# arbitrary 1ms add
			}
			else{
				$sleepdur *= 1.5;
			}
		}
		elsif($curbps < $targkBps){	# We're going too slow...
			if($sleepdur > 0){
				if($sleepdur < 0.00001){	# lowest thresshold at 10us
					$sleepdur = 0;
				}
				else{
					$sleepdur -= ($sleepdur * 0.5);
				}
			}
		}

		if($sleepdur > 0){ 				# throttle if appropriate
			select undef, undef, undef, $sleepdur;
		}
	}
}

#########################################################################################
# Looks for input keys and reconfigures.  This relies on or otherwise uses globals, so 
# is a future candidate for refactorage.
#########################################################################################
sub checkAndDoKeyProcessing {
	ReadMode ('cbreak');
	my $char;
	if (defined ($char = ReadKey(-1)) ) {
		print "\b \b";	# Stomp over key

		if($char =~ /1/){
			$targkBps = $lowbw;
			print "\nSetting bandwidth to low ($lowbw kBps)\n";
			# Reset bandwidth stuff when we change bw modes
			@{$bwinfo{'start'}} = Time::HiRes::gettimeofday();
			$bwinfo{'bytes'} = 0;
			$displine = '';
		}
		elsif($char =~ /2/){
			$targkBps = $medbw;
			print "\nSetting bandwidth to medium ($medbw kBps)\n";
			# Reset bandwidth stuff when we change bw modes
			@{$bwinfo{'start'}} = Time::HiRes::gettimeofday();
			$bwinfo{'bytes'} = 0;
			$displine = '';
		}
		elsif($char =~ /3/){
			$targkBps = 0;	# set to high 
			print "\nSetting bandwidth to maximum setting (no limit)\n";
			# Reset bandwidth stuff when we change bw modes
			@{$bwinfo{'start'}} = Time::HiRes::gettimeofday();
			$bwinfo{'bytes'} = 0;
			$sleepdur = 0;
			$displine = '';
		}
		elsif($char =~ /q/){
			$quitnow = 1;
			print "\n";
		}
		elsif($char =~ /s/){
			print "\nSkipping to next file after finishing segment...\n";
			$displine = '';
			$skipthisfile = 1;
			print "\n";
		}
	}
	ReadMode ('normal');                  # restore normal tty settings
}

#########################################################################################
# build a nice display line
#########################################################################################
sub buildDispLine {
	my ($partnum, $segct, $segbytesread, $segsize, $filebytesread, $filesize, $starttimeref) = @_;

	my $displine = "";
	my $bar = "|";
	my $perc = 0;
	$filesize and $perc = int(($filebytesread/$filesize)*25);
	$bar .= '#' x $perc;
	$bar .= "-" x (25-$perc);
	$bar .= "|";
	$displine .= " " . $bar;
	$displine .= " ";
	if($filesize){
		$displine .= int(($filebytesread/$filesize)*100) . "%";
	}
	else{
		$displine .= "??%";
	}
	$displine .= "  [" . hrsv($filebytesread) . "/" . hrsv($filesize) . "]";
	$displine .= "  [part " . $partnum . "/" . $segct . " " . hrsv($segbytesread) . "/" . hrsv($segsize) . "]";

	my $bw = ($bwinfo{'bytes'}/1024.0)/Time::HiRes::tv_interval($bwinfo{'start'});
	#my $bw = Time::HiRes::tv_interval($starttimeref);
	if($bw > 0){
		#$bw = sprintf("%4.1f", ($filebytesread/1024.0)/$bw);
		$bw = sprintf("%4.1f", $bw);
	}
	$displine .= " { ";
	if($targkBps == 0){
		$displine .= "high: ";
	}
	elsif($targkBps == $medbw){
		$displine .= "med: ";
	}
	elsif($targkBps == $lowbw){
		$displine .= "low: ";
	}
	else{
		$displine .= "wtf: ";
	}

	$displine .= "$bw kB/s }";
	#$displine .= " sleepdur = " . sprintf("%.4f", $sleepdur); #debug extra
	return $displine;
}

#########################################################################################
# human readable size value
#########################################################################################
sub hrsv {
	my $size = shift;  # presumed bytes
	my $k = 1.0*$size/1024;
	my $m = 1.0*$size/(1024*1024);
	my $g = 1.0*$size/(1024*1024*1024);
	if($g > 1){
		print "HERE";
		return sprintf("%0.2fG", $g);
	}
	if($m > 1){
		return sprintf("%0.2fM", $m);
	}
	if($k > 1){
		return sprintf("%dk", $k);
	}
	return $size;
}

#########################################################################################
# read password without echoing it
#########################################################################################
sub readPassword {
	ReadMode 2;	# no echo
	my $pw = <STDIN>;
	ReadMode 0; # default
	print "\n";
	return $pw;
}

#########################################################################################
# Parse NZB file and return array of files
# TODO: The structure returned from this function should really be documented....but
# for now, if you need it, use Dumper to view the format.  Should be self explanatory.
#########################################################################################
sub parseNZB {

	my ($nzbfilename, $sorted) = @_;
	my $parser = new XML::DOM::Parser;
	my @fileset;
	print "Loading and parsing nzb file: " . $nzbfilename . "\n";
	my $nzbdoc = $parser->parsefile($nzbfilename);
	my $files = $nzbdoc->getElementsByTagName("file");
	my $totalsegct = 0;
	foreach my $i (0..$files->getLength()-1){
		my $fileNode = $files->item($i);
		my $subj = $fileNode->getAttributes()->getNamedItem('subject');
		#print $subj->getValue() . "\n";

		my %file;
		$file{'name'} = $subj->getValue();

		my @groupnames;
		for my $group ($fileNode->getElementsByTagName('group')) {
			push @groupnames, $group->getFirstChild()->getNodeValue();
		}
		$file{'groups'} = \@groupnames;

		my @segments;
		for my $seg ($fileNode->getElementsByTagName('segment')) {
			my %seghash;

			my $size = $seg->getAttributes()->getNamedItem('bytes')->getValue();
			$file{'totalsize'} += $size;

			$seghash{'msgid'} = $seg->getFirstChild()->getNodeValue();
			$seghash{'size'} = $size;

			push @segments, \%seghash;
		}
		$totalsegct += scalar @segments;
		$file{'segments'} = \@segments;

		push @fileset, \%file;
	}
	$nzbdoc->dispose;

	if($sorted){
		print "Sorting files by filename (subject)...";
		@fileset = 
			sort {
				#print "Checking $a and $b " . $a->{'name'} . " " . $b->{'name'} . "\n";
				$a->{'name'} cmp $b->{'name'};
			} @fileset;
		print "finished.\n";
			
	}
	print "Loaded $totalsegct total segments for " . $files->getLength() . " file(s).\n";
	
	return @fileset;
}

#########################################################################################
# Show program usage
#########################################################################################
sub showUsage {
print <<EOL

  nzbperl version 0.01 -- usage:

  nzbperl <options> <filename.nzb>

  where <options> are:

  --server <server> : Usenet server to use (defaults to NNTPSERVER env var)
  --user <user>     : Username for server (blank of not needed)
  --pw <pass>       : Password for server (blank to prompt if --user given)
  --keep            : Keep parts files after decoding
  --redo            : Don't skip over files already downloaded, download again
  --med <kBps>      : Set "med" bandwidth to kBps (default is 95kBps)
  --low <kBps>      : Set "low" bandwidth to kBps (default is 35kBps)
  --speed <speed>   : Explicitly specify transfer bandwidth in kBps
  --nosort          : Don't sort files by name before processing
  --help            : Show this screen

   During runtime, the following keys may be used:
     1 -- set bandwidth to low
     2 -- set bandwidth to med
     3 -- set bandwidth to high (unlimited)

  nzbperl version 0.01, Copyright (C) 2004 Jason Plumb
  nzbperl comes with ABSOLUTELY NO WARRANTY; This is free software, and 
  you are welcome to redistribute it under certain conditions;  Please 
  see the source for additional details.

EOL
}

