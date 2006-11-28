#!/usr/bin/perl
#
# nzbperl.pl -- version 0.5
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
#   * Consider putting tempfile (.parts files) in /tmp or other customize location
#     perhaps with parameter
#   * User specified output directory.
#   * Possibly doing regex filtering on filenames in nzb file?

use strict;
use Socket;
use XML::DOM;
use Getopt::Long;
use Time::HiRes;	# timer stuff
use Term::ReadKey;	# for no echo password reading
use Term::Cap;

my $version = '0.5';
my $ospeed;
my $terminal = Tgetent Term::Cap { TERM => undef, OSPEED => $ospeed };
my $recv_chunksize = 5*1024;	# How big of chunks we read at once from a connection (this is pulled from ass)

my $dispchunkct = 250;			# Number of data lines to read between screen updates.
my $targkBps = 0;
my ($medbw, $lowbw) = (95, 35);	# Defaults for low and medium speed settings.
my $sleepdur = 0;				# Used when throttling

# Make stdout not buffered.
my $old_fh = select(STDOUT);
$| = 1;
select($old_fh);

my $quitnow = 0;
my $showinghelpscreen = 0;
my $skipthisfile = 0;
my $usecolor = 1;
my ($server, $user, $pw, $keep, $help, $nosort, $overwritefiles, $connct, $nocolor) =
	('', '', '', 0, 0, 0, 0, 2, 0);
GetOptions( 'server=s' => \$server, 'user=s' => \$user, 'pw=s' => \$pw, 'help' => \$help,
			'med=s' => \$medbw, 'low=s' => \$lowbw, 'speed=s' => \$targkBps, 
			'keep' => \$keep, 'nosort' => \$nosort, 'redo' => \$overwritefiles, 
			'conn=i' => \$connct, 'nocolor' => \$nocolor);
if($help){
	showUsage();
	exit 1;
}
if($keep){
	$keep = '';
}
else {
	$keep = '-c';
}
$nocolor and $usecolor = 0;
if(not $nocolor){
	use Term::ANSIColor;
}


if($user and !$pw){
	print "Password for '$user': ";
	$pw = readPassword();
}
if(not $ARGV[0]){		# No NZB file given?
	showUsage(); 
	exit;
}

if(not length($server)){
	$server = $ENV{'NNTPSERVER'};
	not $server and die "Must provide --server or set \$NNTPSERVER environment\n";
}

my $nzbfilename = $ARGV[0];
my @fileset = parseNZB($nzbfilename, !$nosort);
my %totals;
$totals{'total size'} = computeTotalNZBSize(@fileset);
my @statusmsgs;
statMsg('Welcome -- nzbperl started!');

# Descriptions of what's in the connection hash (for sanity sake)
#
# $conn->{'sock'}       : the socket for comms
# $conn->{'msg'}        : message that describes what's going on
# $conn->{'file'}       : the file it's working on
# $conn->{'segnum'}     : the segment number of the file it's working on
# $conn->{'segbytes'}   : number of bytes read in the current segment
# $conn->{'filebytes'}  : number of bytes read in the current file
# $conn->{'bstatus'}    : status about how we're handling a body (starting
# $conn->{'buff'}       : where body data is buffered
# $conn->{'tmpfilename'}: temporary file name
# $conn->{'tmpfile'}    : temporary file handle
# $conn->{'bwstarttime'}: time when the bandwdith applied
# $conn->{'bwstartbytes'}: bytes read on file when bandwidth applied
# $conn->{'truefname'}  : true filename on disk (assumed after decoding)
# $conn->{'skipping'}   : indicates we're in the middle of a skipping operation

displayShortGPL();

my @lastdrawtime = Time::HiRes::gettimeofday();
my @conn;
createConnections();
if($user){
	doLogins() or die "Error authenticating to server.\nPlease check the user/pass info and try again.";
}

$terminal->Tputs('cl', 1, *STDOUT);			# clears screen
my ($oldwchar, $wchar, $hchar, $wpixels, $hpixels);  	# holds screen size info

my @queuefileset = @fileset;
my @dlstarttime = Time::HiRes::gettimeofday();
while(1){

	doFileAssignments();
	doBodyRequests();

	doReceiverPart();

	# See if queuefileset is empty AND all sockets don't have files
	# when that happens, that's when we're done.
	if(not scalar @queuefileset){
		doBodyRequests();		# total hack, but that's where decoding happens...
		my $done = 1;
		foreach my $i (1..$connct){
			if($conn[$i-1]->{'file'}){
				$done = 0;
				last;
			}
		}
		if($done){
			$terminal->Tgoto('cm', 0, 1+5+(3*$connct)+9, *STDOUT);
			print "All downloads complete!\n";
			last;
		}
	}
	$quitnow and last;
}

$terminal->Tgoto('cm', 0, (1+5+(3*$connct)+8), *STDOUT);

if($quitnow){# Do some cleanups
	foreach my $c (@conn){
		next unless $c->{'file'};
		if($c->{'tmpfile'}){
			print "Closing and deleting " . $c->{'tmpfilename'} . "...\n";
			close $c->{'tmpfile'};
			unlink $c->{'tmpfilename'};
		}
	}
}
disconnectAll();
pc("Thanks for using ", 'bold yellow');
pc("nzbperl", 'bold red');
pc("! Enjoy!\n\n", 'bold yellow');

#########################################################################################
# Does multiplexed comms receiving
#########################################################################################
sub doReceiverPart {
	my ($rin, $ein) = ('', '');
	my ($rout, $eout);

	foreach my $i (1..$connct){
		vec($rin, fileno($conn[$i-1]->{'sock'}), 1) = 1;
		vec($ein, fileno($conn[$i-1]->{'sock'}), 1) = 1;
	}

	my $nfound = select($rout=$rin, undef, $eout=$ein, 0.25);  

	foreach my $i (1..$connct){
		if(vec($rout, fileno($conn[$i-1]->{'sock'}),1) == 1){
			recv $conn[$i-1]->{'sock'}, my $buff, $recv_chunksize, undef;
			$conn[$i-1]->{'buff'} .= $buff;
			if($conn[$i-1]->{'bstatus'} =~ /starting/){
				# We're just starting, need to slurp up 222 response
				if($conn[$i-1]->{'buff'} =~ /^2\d\d\s.*\r\n/s){
					$conn[$i-1]->{'buff'} =~ s/2\d\d\s.*\r\n//;
					$conn[$i-1]->{'segbytes'} = length($conn[$i-1]->{'buff'});
					#print "DEBUG: We got the 222 (or whatever) server response...\n";
					$conn[$i-1]->{'bstatus'} = 'running';
				}
			}
			else{
				$conn[$i-1]->{'segbytes'} += length($buff);
				$conn[$i-1]->{'filebytes'} += length($buff);
				$totals{'total bytes'} += length($buff);
			}

			# Spool all lines from the buffer into the output file.
			while(1){
				my $ind1 = index $conn[$i-1]->{'buff'}, "\r\n";
				last unless $ind1 >= 0;
				my $line = substr $conn[$i-1]->{'buff'}, 0, $ind1+2, '';
				$line =~ s/^\.\././o;

				# Try and detect the "real" filename
				if(not $conn[$i-1]->{'truefname'}){
					my $tfn = getTrueFilename($line);
					$tfn and ($conn[$i-1]->{'truefname'} = $tfn);
					$tfn and statMsg("Conn. $i: Found true filename: $tfn");

					if(fileExists($tfn)){
						if(!$overwritefiles){
							# We can't just close and delete, because there will likely still be 
							# data waiting in the receive buffer.  As such, we have to set a flag
							# to indicate that the file already exists and should be skipped...
							# This is perhaps a bit silly -- we have to finish slurping in the
							# BODY part before we can start working on the next file...
							statMsg("File already exists on disk (skipping after segment completes)");
							$conn[$i-1]->{'skipping'} = 1;
						}
					}
					
				}
				
				if($line =~ /^\.\r\n/){		# detect end of BODY..
					$conn[$i-1]->{'bstatus'} = 'finished';
					if($conn[$i-1]->{'skipping'}){
						close $conn[$i-1]->{'tmpfile'};
						unlink $conn[$i-1]->{'tmpfilename'};
						$conn[$i-1]->{'file'} = undef;
						$conn[$i-1]->{'skipping'} = undef;	# no longer skipping (for next time)
					}
					last;
				}
				else{
					print {$conn[$i-1]->{'tmpfile'}} $line;
				}
			}
		}
		drawScreenAndHandleKeys();
		doThrottling();
	}
}

#########################################################################################
# Heuristically determines the "true" filename.  Returns filename or undef
#########################################################################################
sub getTrueFilename {
	my $line = shift;
	$line =~ s/\s+$//;
	if($line =~ /^=ybegin/){			# Yenc candidate
		# I'm assuming that the last tag on the line is "name=...", which I honestly have no idea
		# if that's always true.  :)
		$line =~ s/.* name=//;
		return $line;
	}
	elsif($line =~ /^begin \d+ /){		# UUencoded candidate
		$line =~ s/^begin \d+ //;
		return $line;
	}
	else{
		return undef;
	}
}

#########################################################################################
# Tries to see if the file exists on disk
#########################################################################################
sub fileExists {
	my $fname = shift;
	# Have to escape special chars so that grep will work on funky ass filenames
	$fname =~ s/([\(\)\{\}\[\]\<\>\+\?\.\*\$\|\\\/])/\\$1/g;

	opendir(DIR, ".") or die "Error checking output path for file existence: $!";
	my @match = grep(/^$fname$/, readdir(DIR));
	closedir(DIR);
	if(scalar(@match) > 0){			# We have the file
		return 1;
	}
	return 0;
}
#########################################################################################
# Handles segments and detects when we're done with a file
#########################################################################################
sub doBodyRequests {
	foreach my $i (1..$connct){
		my $conn = $conn[$i-1];
		my $file = $conn->{'file'};
		next unless $file;			# Bail if we don't have a file
		if($conn->{'segnum'} < 0){
			$conn->{'segnum'} = 0;
			my $seg = @{$file->{'segments'}}[0];
			$conn->{'seg'} = $seg;
			my $msgid = $seg->{'msgid'};
			send $conn->{'sock'}, 'BODY <' . $msgid . ">\r\n", undef;
			$conn->{'bstatus'} = 'starting';
		}
		elsif($conn->{'bstatus'} =~ /finished/){ # finished a segment
			$conn->{'segnum'}++;

			if($conn->{'segnum'} >= scalar @{$file->{'segments'}}){ # All segments for this file exhausted.
				close $conn[$i-1]->{'tmpfile'};
				my $tmpfilename = $conn[$i-1]->{'tmpfilename'};
				showDecodingStartStopMsg($i-1, 'start');
				`uudeview -i -a $keep -q "$tmpfilename" 2>&1>/tmp/nzbdbgout.txt`;
				#my $rcode = system('uudeview -i -a $keep -q "$tmpfilename"');
				#statMsg("DEBUG: return code was $rcode");

				# TODO: Check return value or otherwise confirm that the decode actually worked.
				statMsg("Completed decode of " . $conn[$i-1]->{'truefname'});

				$totals{'finished files'}++;
				$conn->{'file'} = undef;
				$conn->{'seg'} = undef;

			}
			else{
				my $segnum = $conn->{'segnum'};
				my $seg = @{$file->{'segments'}}[$segnum];
				$conn->{'seg'} = $seg;
				my $msgid = $seg->{'msgid'};
				send $conn->{'sock'}, 'BODY <' . $msgid . ">\r\n", undef;
				$conn->{'bstatus'} = 'starting';
			}
		}
	}
}
#########################################################################################
# Shifts from the file queue and assigns the files to a connection.  When a file is
# assigned, the first segment is not assigned.
#########################################################################################
sub doFileAssignments {
	foreach my $i (1..$connct){
		my $conn = $conn[$i-1];
		next if $conn->{'file'};	# already working on a file
		my $file = shift @queuefileset;
		last unless $file;
		statMsg(sprintf("Conn. %d starting file: %s", $i, $file->{'name'}));
		$conn->{'file'} = $file;
		$conn->{'segnum'} = -1;
		$conn->{'filebytes'} = 0;
		$conn->{'truefname'} = undef;
		$conn->{'bwstartbytes'} = 0;
		#@{$conn->{'fstarttime'}} = Time::HiRes::gettimeofday();
		@{$conn->{'bwstarttime'}} = Time::HiRes::gettimeofday();

		# Create temp filename and open
		my $tmpfile = 'nzbperl.tmp' . time . '.' . $i . '.parts';
		$conn->{'tmpfilename'} = $tmpfile;

		open $conn->{'tmpfile'}, '>' . $tmpfile or 
			(statMsg("*** ERROR opening $tmpfile (critical!)") and next);
		statMsg("Opened temp file $tmpfile");
		binmode $conn->{'tmpfile'};
	}
}

#########################################################################################
# Creates all connections and adds them to the @conn global
#########################################################################################
sub createConnections {

	my $port = 119;	# todo: make me configurable
	my $iaddr = inet_aton($server) || die "Error resolving host: $server";
	my $paddr = sockaddr_in($port, $iaddr);

	foreach my $i (1..$connct){
		my $sock;
		socket($sock, PF_INET, SOCK_STREAM, getprotobyname('tcp')) || die "Error creating socket: $!";
		print "Attempting connection #$i to $server...";
		connect($sock, $paddr) || die "Error connecting: $!";
		print "success!\n";
		my $line = blockReadLine($sock);	# read server connection/response string
		not $line =~ /^(200|201)/ and die "Unexpected server response: $line" . "Expected 200 or 201.\n";
		$conn[$i-1]->{'sock'} = $sock;
	}
	return 1;
}

#########################################################################################
# Attempts to perform a login on each connection
#########################################################################################
sub doLogins {
	foreach my $i (1..$connct){
		my $sock = $conn[$i-1]->{'sock'};
		print "Attempting login on connection #$i...";
		send $sock, "AUTHINFO USER $user\r\n", undef;
		my $line = blockReadLine($sock);
		if($line =~ /^381/){
			send $sock, "AUTHINFO PASS $pw\r\n", undef;
			$line = blockReadLine($sock);
			$line =~ s/\r\n//;
			(not $line =~ /^281/) and print ">FAILED<\n* Authentication to server failed: ($line)\n" and exit(0);
			print "success!\n";
		}
		elsif($line =~ /^281/){ # not sure if this happens, but this means no pw needed I guess
			print "no password needed, success!\n";
		}
		else {
			print "server returned: $line\n";
			die ">LOGIN FAILED<\n";
		}
	}
	return 1;
}

#########################################################################################
# Computes and returns the total speed for this session.
#########################################################################################
sub getTotalSpeed {
	my $runtime = Time::HiRes::tv_interval(\@dlstarttime);
	return uc(hrsv($totals{'total bytes'}/$runtime));
}

#########################################################################################
# Looks at all the current connections and calculates a "current" speed
#########################################################################################
sub getCurrentSpeed {
	my $sumbps = 0;
	my $suppresshsrv = shift;
	foreach my $i (1..$connct){
		my $c = $conn[$i-1];
		next unless $c->{'file'};	# skip inactive connections
		$sumbps += ($c->{'filebytes'} - $c->{'bwstartbytes'})/Time::HiRes::tv_interval($c->{'bwstarttime'});
	}
	$suppresshsrv and return $sumbps;
	return uc(hrsv($sumbps));
}
#########################################################################################
# gets the estimated ETA in hrs:mins:secs
#########################################################################################
sub getETA {
	my ($h, $m, $s);
	my $curspeed = getCurrentSpeed(1); # in bytes/sec
	not $curspeed and return "??:??:??";
	my $remainbytes = $totals{'total size'} - $totals{'total bytes'};
	my $etasec = $remainbytes / $curspeed;
	$h = int($etasec/(60*60));
	$m = int(($etasec-(60*60*$h))/60);
	$s = $etasec-(60*60*$h)-(60*$m);
	return sprintf("%.2d:%.2d:%.2d", $h, $m, $s);
}

#########################################################################################
# Checks the last paint time and updates the screen if necessary.  Also checks for 
# keyboard keys.
#########################################################################################
sub drawScreenAndHandleKeys {
	if($showinghelpscreen){
		$terminal->Tgoto('cm', 40, 13, *STDOUT);
		print "ETA: " . getETA() . ")";
		$terminal->Tgoto('cm', 0, (1+5+(3*$connct)+8), *STDOUT);
	}
	elsif(Time::HiRes::tv_interval(\@lastdrawtime) > 0.5){	# Refresh screen every 0.5sec max
		($wchar, $hchar, $wpixels, $hpixels) = GetTerminalSize();
		if($oldwchar != $wchar){
			$oldwchar and statMsg("Terminal was resized (new width = $wchar), redrawing");
			$terminal->Tputs('cl', 1, *STDOUT);			# clears screen
			drawBorder();
		}
		$oldwchar = $wchar;
		@lastdrawtime = Time::HiRes::gettimeofday();
		drawHeader();
		drawConnInfos();
		drawStatusMsgs();

		$terminal->Tgoto('cm', 0, (1+5+(3*$connct)+8), *STDOUT);
		print "'?' for help> ";
	}
	ReadMode ('cbreak');
	my $char;
	while (defined ($char = ReadKey(-1)) ) {	# have a key
		handleKey($char);
	}
	ReadMode ('normal');                  # restore normal tty settings
}

#########################################################################################
# Does bandwidth throttling
#########################################################################################
sub doThrottling {
	not $targkBps and return;		# Max setting, don't throttle.
	$quitnow and return;			# Don't bother if quitting
	my $curbps = getCurrentSpeed(1)/1024; # in kBps
	# TODO: Using percentages could likely make this way better.
	# (ie. inc/dec sleep duration by error percentage %)
	if($curbps > $targkBps){		# We're going too fast...
		if($sleepdur == 0){
			$sleepdur = 0.001;		# arbitrary 1ms add
		}
		else{
			$sleepdur *= 1.5;
		}
	}
	elsif($curbps < $targkBps){
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


#########################################################################################
# Shows a decoding starting message for a connection
#########################################################################################
sub showDecodingStartStopMsg {
	my $i = shift;
	my $st = shift;
	my $startrow = 6;
	my $len = 0;
	$terminal->Tgoto('cm', 2, $startrow+(3*($i)), *STDOUT);
	$len += pc(sprintf("%d: ", $i+1), 'bold white');
	if($st =~ /start/){
		$len += pc('Starting decode of ' . $conn[$i]->{'truefname'}, 'bold red');
	}
	else{
		$len += pc('Decode of ' . $conn[$i]->{'truefname'} . " complete", 'bold red');
	}
	print ' ' x ($wchar-$len-4);
	$terminal->Tgoto('cm', 2, $startrow+(3*($i))+1, *STDOUT);
	print ' ' x ($wchar-4);
}

#########################################################################################
# Handles a keypress
#########################################################################################
sub handleKey {

	if($showinghelpscreen){
		$showinghelpscreen = 0;
		$terminal->Tputs('cl', 1, *STDOUT);			# clears screen
		$oldwchar = 0;		# Hack to force border(s) to be redrawn
		return;	# cancel help screen display
	}

	my $key = shift;
	if($key =~ /q/){
		$quitnow = 1;
		statMsg("User forced quit...exiting...");
		# TODO: Close open files and delete parts files.
		drawStatusMsgs();
		updateBWStartPts();
	}
	elsif($key =~ /1/){
		$targkBps = $lowbw;
		statMsg("Setting bandwidth to low value ($lowbw" . "kBps)");
		drawStatusMsgs();
		updateBWStartPts();
	}
	elsif($key =~ /2/){
		$targkBps = $medbw;
		statMsg("Setting bandwidth to medium value ($medbw" . "kBps)");
		drawStatusMsgs();
		updateBWStartPts();
	}
	elsif($key =~ /3/){
		$targkBps = 0;	# set to high 
		statMsg("Setting bandwidth to maximum (unlimited)");
		drawStatusMsgs();
		updateBWStartPts();
	}
	elsif($key =~ /h/ or $key =~ /\?/){
		statMsg("Displaying help screen at user's request");
		showHelpScreen();
	}
	elsif($key =~ /c/){
		$usecolor = $usecolor ^ 0x01; #invert value
	}
	elsif($key =~ /\+/){
		$targkBps++;
		statMsg("Nudging bandwidth setting up to " . $targkBps . "kBps");
		drawStatusMsgs();
		updateBWStartPts();
	}
	elsif($key =~ /-/){
		if(!$targkBps){
			$targkBps = int(getCurrentSpeed(1)/1024)-1;
			statMsg("Nudging bandwidth setting down to " . $targkBps . "kBps");
			
		}
		else{
			$targkBps--;
			statMsg("Nudging bandwidth setting down to " . $targkBps . "kBps");
		}
		drawStatusMsgs();
		updateBWStartPts();
	}
	else {
		statMsg("Unknown key: $key (try 'h' for help)");
	}
}


#########################################################################################
# When the bandwidth changes, update all bw baselines for all connections
#########################################################################################
sub updateBWStartPts {
	foreach my $i (1..$connct){
		my $c = $conn[$i-1];
		$c->{'bwstartbytes'} = $c->{'filebytes'};
		@{$c->{'bwstarttime'}} = Time::HiRes::gettimeofday();
	}
}

#########################################################################################
# Draws the header that contains summary info etc.
#########################################################################################
sub drawHeader(){
	$terminal->Tgoto('cm', 2, 1, *STDOUT);
	pc("nzbperl v.$version", 'bold red');
	pc(" :: ", 'bold white');
	#pc("by jason plumb ", 'bold red');
	#pc("and ", 'bold white');
	pc("noisybox.net", 'bold red');

	my $len = 0;
	$terminal->Tgoto('cm', 2, 3, *STDOUT);
	$len += pc("Files remaining: ", 'bold white');
	$len += pc((scalar @fileset)-$totals{'finished files'}, 'bold green');
	$len += pc(" of ", 'white');
	$len += pc(scalar(@fileset), 'bold green');
	my $dlperc = int(100.0*$totals{'total bytes'} / $totals{'total size'});
	$len += pc(' [', 'bold blue');
	$len += pc(hrsv($totals{'total bytes'}) . 'B', 'bold green');
	$len += pc('/', 'bold white');
	$len += pc(hrsv($totals{'total size'}) . 'B', 'bold green');
	$len += pc(']', 'bold blue');
	$len += pc(" ", 'white');
	$len += pc($dlperc. '%', 'bold yellow');
	$len += pc("  ETA: ", 'bold white');
	$len += pc(getETA(), 'bold yellow');
	print (' ' x ($wchar-$len-4));

	$terminal->Tgoto('cm', 2, 2, *STDOUT);
	$len = pc("Current speed: ", 'bold white');
	$len += pc(getCurrentSpeed() . "Bps", 'bold green');
	$len += pc(" (", 'bold blue');
	$len += pc("target ", 'white');
	$len += pc(' = ', 'white'); 
	if($targkBps){
		$len += pc(hrsv($targkBps*1024) . "Bps", 'bold green');
	}
	else{
		$len += pc("unlimited!", 'bold red');
	}
	$len += pc(")", 'bold blue');
	$len += pc("  Session speed: ", 'bold white');
	$len += pc(getTotalSpeed() . "Bps", 'bold green');
	print (' ' x ($wchar-$len-4));
}


#########################################################################################
# Draws statuses for all individual connections
#########################################################################################
sub drawConnInfos(){
	my $startrow = 6;
	foreach my $i(1..$connct){
		my $conn = $conn[$i-1];

		if(not $conn->{'file'}){
			if(scalar(@queuefileset) == 0){
				# This connection has no more work to do...
				$terminal->Tgoto('cm', 2, $startrow+(3*($i-1)), *STDOUT);
				my $len = pc(sprintf("%d: Nothing left to do...", $i), 'bold cyan');
				print (' ' x ($wchar-$len-4));
				$terminal->Tgoto('cm', 2, $startrow+(3*($i-1))+1, *STDOUT);
				$len = pc("   <waiting for others to finish>", 'bold cyan');
				print (' ' x ($wchar-$len-4));
			}
			next;
		}

		my $file = $conn->{'file'};
		my $filesize = $file->{'totalsize'};
		my $filebytesread = $conn->{'filebytes'};
		my $segnum = $conn->{'segnum'}+1;
		my $segct = scalar @{$conn->{'file'}->{'segments'}};
		my $segbytesread = $conn->{'segbytes'};
		my $cursegsize = @{$conn->{'file'}->{'segments'}}[$segnum-1]->{'size'};

		$terminal->Tgoto('cm', 2, $startrow+(3*($i-1)), *STDOUT);

		my $len = 
			pc(sprintf("%d: Downloading: ", $i), 'bold white');
		my $fn = $file->{'name'};
		if( length($fn) + $len > $wchar-4){
			$fn = substr($fn, 0, $wchar-4-$len);
		}
		
		pc($fn, 'white');

		$terminal->Tgoto('cm', 2, $startrow+(3*($i-1))+1, *STDOUT);
		my $perc = 0;
		$filesize and $perc = int(($filebytesread/$filesize)*25);
		$len = pc("   |", 'bold white');
		if($perc){
			$len += pc('#' x ($perc-1), 'bold white');
			$len += pc('#', 'bold red');
		}
		$len += pc('-' x (25-$perc), 'white');
		$len += pc("| ", 'bold white');
		if($filesize){
			$len += pc(int(($filebytesread/$filesize)*100) . "%", 'bold yellow');
		}
		else{
			$len += pc("??%", 'bold yellow');
		}
		$len += pc("  [", 'bold white');
		$len += pc(hrsv($filebytesread), 'bold green');
		$len += pc("/", 'bold white');
		$len += pc(hrsv($filesize), 'bold green');
		$len += pc("]", 'bold white');
		$len += pc("  [part ", 'bold white');
		$len += pc($segnum, 'bold cyan');
		$len += pc("/", 'bold white');
		$len += pc($segct, 'bold cyan');
		$len += pc(" ", '');
		$len += pc(hrsv($segbytesread), 'bold cyan');
		$len += pc("/", 'bold white');
		$len += pc(hrsv($cursegsize), 'bold cyan');
		$len += pc("]", 'bold white');

		print ' ' x ($wchar - $len - 4);

	}

}

#########################################################################################
sub drawStatusMsgs {
	# TODO:  Consider saving state about status messages -- could save cycles by not
	#        automatically drawing every time.
	my $row = 3*$connct + 6 + 1;
	my $statuslimit = 6;	# number of lines to show.

	# Trim status messages to size
	while( scalar(@statusmsgs) > $statuslimit){
		shift @statusmsgs;
	}
	foreach my $line (@statusmsgs){
		$terminal->Tgoto('cm', 2, $row, *STDOUT);
		if(length($line) > $wchar-4){
			$line = substr($line, 0, $wchar-4);	# Clip line
		}
		else{
			$line .= (' ' x ($wchar-4-length($line)));
		}
		pc($line, 'white');
		$row++;
	}
	$terminal->Tgoto('cm', 0, (1+5+(3*$connct)+8), *STDOUT);
	print "'?' for help> ";
}

#########################################################################################
# Draws a border around the screen.
#########################################################################################
sub drawBorder {
	drawVLine(0);
	drawVLine($wchar);
	drawHLine(0);
	drawHLine(4);
	drawHLine(1+5+(3*$connct));
	drawHLine(1+5+(3*$connct)+7);
}

sub drawHLine {
	my $ypos = shift;
	$terminal->Tgoto('cm', 0, $ypos, *STDOUT);
	pc('+' . ('-' x ($wchar-2)) . '+', 'bold white');
}
sub drawVLine {
	my $xpos = shift;
	my $height = shift;
	not $height and $height = (1+5+(3*$connct)+7);
	foreach(0..$height){
		$terminal->Tgoto('cm', $xpos, $_, *STDOUT);
		pc('|', 'bold white');
	}
}

#########################################################################################
# helper for printing in color (or not)
#########################################################################################
sub pc {
	my ($string, $colstr) = @_;
	if($usecolor){
		print colored ($string, $colstr);
	}
	else{
		print $string;
	}
	return length($string);
}

#########################################################################################
# Adds a status message with timestamp
#########################################################################################
sub statMsg {
	my $str = shift;
	my @t = localtime;
	my $msg = sprintf("%0.2d:%0.2d - %s", $t[2], $t[1], $str);
	push @statusmsgs, $msg;
	return 1;
}

#########################################################################################
# Reads a line from the socket in a blocking manner.
#########################################################################################
sub blockReadLine {
	my $sock = shift;
	my ($line, $buff) = ('', '');
	while(1){
		sysread $sock, $buff, 1024 or last;
		$line .= $buff;
		last if $line =~ /\r\n$/;
	}
	return $line;
}


#########################################################################################
# Gracefully close down all server connections.
#########################################################################################
sub disconnectAll {
	foreach my $i (1..$connct){
		my $sock = $conn[$i-1]->{'sock'}, 
		print "Closing down connection #$i...";
		send $sock, "QUIT\r\n", undef;
		my $line = blockReadLine($sock);
		$line =~ /^205/ and print "closed gracefully!";
		print "\n";
		close $sock;
		$conn[$i-1]->{'sock'} = undef;
	}
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
# Determines the total file size for all segments in the NZB file
#########################################################################################
sub computeTotalNZBSize {
	my @fileset = @_;
	my $tot = 0;
	foreach my $file (@fileset){
		foreach my $seg (@{$file->{'segments'}}){
			$tot += $seg->{'size'};
		}
	}
	return $tot;
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
				$a->{'name'} cmp $b->{'name'};
			} @fileset;
		print "finished.\n";
			
	}
	print "Loaded $totalsegct total segments for " . $files->getLength() . " file(s).\n";
	
	return @fileset;
}

#########################################################################################
sub displayShortGPL {
	print <<EOL

  nzbperl version $version, Copyright (C) 2004 Jason Plumb
  nzbperl comes with ABSOLUTELY NO WARRANTY; This is free software, and 
  you are welcome to redistribute it under certain conditions;  Please 
  see the source for additional details.

EOL
;
}

#########################################################################################
# Shows a help screen for interactive keys
#########################################################################################
sub showHelpScreen {
	$terminal->Tputs('cl', 1, *STDOUT);			# clears screen
	print <<EOL

  Hi.  This is the nzbperl help screen. 
  You can use the following keys while we're running:

  '1'   : Switch to low bandwidth mode ($lowbw kBps)
  '2'   : Switch to med bandwidth mode ($medbw kBps)
  '3'   : Switch to high bandwidth mode (unlimited)
  '+'   : Nudge target bandwidth setting up 1 kBps
  '-'   : Nudge target bandwidth setting down 1 kBps
  'c'   : Toggle color on or off
  'q'   : Quit the program (aborts all downloads)
  '?'   : Show this help screen

  (Your download is still in progress:  
  
  [ Press any key to return to the main screen ]

EOL
;
	drawVLine(0, 17);
	drawVLine($wchar, 17);
	drawHLine(0);
	drawHLine(17);

	$terminal->Tgoto('cm', 40, 13, *STDOUT);
	print "ETA: " . getETA() . ")";
	$showinghelpscreen = 1;
}

#########################################################################################
# Show program usage
#########################################################################################
sub showUsage {
print <<EOL

  nzbperl version $version -- usage:

  nzbperl <options> <filename.nzb>

  where <options> are:

  --server <server> : Usenet server to use (defaults to NNTPSERVER env var)
  --user <user>     : Username for server (blank of not needed)
  --pw <pass>       : Password for server (blank to prompt if --user given)
  --conn <n>        : Use <n> server connections (default = 2)
  --keep            : Keep parts files after decoding
  --redo            : Don't skip over files already downloaded, download again
  --med <kBps>      : Set "med" bandwidth to kBps (default is 95kBps)
  --low <kBps>      : Set "low" bandwidth to kBps (default is 35kBps)
  --speed <speed>   : Explicitly specify transfer bandwidth in kBps
  --nosort          : Don't sort files by name before processing
  --nocolor         : Don't use color
  --help            : Show this screen

  During runtime, press 'h' or '?' to see a list of key commands.

  nzbperl version $version, Copyright (C) 2004 Jason Plumb
  nzbperl comes with ABSOLUTELY NO WARRANTY; This is free software, and 
  you are welcome to redistribute it under certain conditions;  Please 
  see the source for additional details.

EOL
}

