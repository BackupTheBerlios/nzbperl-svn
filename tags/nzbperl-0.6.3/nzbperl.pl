#!/usr/bin/perl
#
# nzbperl.pl -- version 0.6.3
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
#   * Other items listed on the project webpage :-)

use strict;
use threads;
use Thread::Queue;
use Socket;
use XML::DOM;
use Getopt::Long;
use Time::HiRes;	# timer stuff
use Term::ReadKey;	# for no echo password reading
use Term::Cap;

my $version = '0.6.3';
my $ospeed;
my $terminal = Tgetent Term::Cap { TERM => undef, OSPEED => $ospeed };
my $recv_chunksize = 5*1024;	# How big of chunks we read at once from a connection (this is pulled from ass)
my $DECODE_DBG_FILE = '/tmp/nzbdbgout.txt';
my $UPDATE_URL = 'http://noisybox.net/computers/nzbperl/nzbperl_version.txt';

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
my $logfile;
my ($server, $port, $user, $pw, $keepparts, $keepbroken, $keepbrokenbin, $help, $nosort, 
    $overwritefiles, $connct, $nocolor, $insane, 
	$dropbad, $skipfilect, $reconndur, $filterregex, 
	$configfile, $uudeview, $daemon, $dlrelative, $dlpath, $noupdate) =
	('', 119, '', '', 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 300, undef, "$ENV{HOME}/.nzbperlrc", undef, 
	0, undef, undef, 0);

# How commandline args are mapped to vars.  This map is also used by config file processor
my %optionsmap = ('server=s' => \$server, 'user=s' => \$user, 'pw=s' => \$pw, 
				'help' => \$help, 'med=s' => \$medbw, 'low=s' => \$lowbw, 
				'speed=s' => \$targkBps, 'keepparts' => \$keepparts, 
				'keepbroken' => \$keepbroken, 'keepbrokenbin' => \$keepbrokenbin, 
				'nosort' => \$nosort, 'redo' => \$overwritefiles, 'conn=i' => \$connct, 
				'nocolor' => \$nocolor, 'log=s' => \$logfile,
				'insane' => \$insane, 'dropbad' => \$dropbad,
				'skip=i' => \$skipfilect, 'retrywait=i' => \$reconndur,
				'filter=s' => \$filterregex, 'config=s' => \$configfile,
				'uudeview=s' => \$uudeview, 'daemon' => \$daemon,
				'dlrelative' => \$dlrelative, 'dlpath=s' => \$dlpath, 'noupdate' => \$noupdate);

handleCommandLineOptions();
if(not $nocolor){
	use Term::ANSIColor;
}
if(not haveUUDeview()){	# Verify that uudeview is installed
	pc("* Please install and configure uudeview and try again.\n", "bold red");
	exit 1;
}

displayShortGPL();
checkForNewVersion();

if($user and !$pw){
	print "Password for '$user': ";
	$pw = readPassword();
}

my @fileset;
while(scalar(@ARGV) > 0){
	my $nzbfilename = shift @ARGV; #$ARGV[0];
	push @fileset, parseNZB($nzbfilename);
}
sortFilesBySubject();	# It checks $sort inside
regexAndSkipping();		# It checks options inside too

print "Looks like we've got " . scalar @fileset . " possible files ahead of us.\n";

my @suspectFileInd;
if($insane){
}
else{
	doNZBSanityChecks();
	if($dropbad){
		dropSuspectFiles();
	}
}

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
# $conn->{'bstatus'}    : status about how we're handling a body (starting/finishing)
# $conn->{'buff'}       : where body data is buffered
# $conn->{'tmpfilename'}: temporary file name
# $conn->{'tmpfile'}    : temporary file handle
# $conn->{'bwstarttime'}: time when the bandwdith applied
# $conn->{'bwstartbytes'}: bytes read on file when bandwidth applied
# $conn->{'truefname'}  : true filename on disk (assumed after decoding)
# $conn->{'skipping'}   : indicates we're in the middle of a skipping operation
# $conn->{'last data'}  : time when data was last seen on this channel
# $conn->{'sleep start'}: time that we started sleeping (for retries)
# $conn->{'isbroken'}   : set when one or more parts fails to download

$totals{'total file ct'} = scalar @fileset;

my @lastdrawtime = Time::HiRes::gettimeofday();
my @conn;
createConnections();
if($user){
	doLogins() or die "Error authenticating to server.\nPlease check the user/pass info and try again.";
}

if($daemon){
	print "nzbperl is running in --daemon mode, output suppressed\n";
	print "Check log for additional details during run...\n";
	my $pid = fork;
	$pid and print "Daemon started [$pid]\n" and exit;
	#close STDIN;
	#close STDOUT;
	#close STDERR;
}

# Start up the decoding thread...
my $decMsgQ = Thread::Queue->new;	# For status msgs
my $decQ = Thread::Queue->new;

my $decThread = threads->new(\&file_decoder_thread);

clearScreen();
my ($oldwchar, $wchar, $hchar, $wpixels, $hpixels);  	# holds screen size info

unlink $DECODE_DBG_FILE;  # Clean up on each run

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
			cursorPos(0, 1+5+(3*$connct)+9);
			pc("All downloads complete!\n", 'bold white');
			last;
		}
	}
	$quitnow and last;
}

cursorPos(0, (1+5+(3*$connct)+8));

if($quitnow){# Do some cleanups
	foreach my $c (@conn){
		next unless $c->{'file'};
		if($c->{'tmpfile'}){
			pc("Closing and deleting " . $c->{'tmpfilename'} . "...\n", 'bold white');
			close $c->{'tmpfile'};
			unlink $c->{'tmpfilename'};
		}
	}
}
disconnectAll();
pc("Waiting for file decoding thread to terminate...\n", 'bold white');
$decQ->enqueue('quit now');
$decThread->join;
pc("Thanks for using ", 'bold yellow');
pc("nzbperl", 'bold red');
pc("! Enjoy!\n\n", 'bold yellow');

#########################################################################################
# This is the thread that does file decoding
# It uses two queues for communication -- decQ for files to decode, decMsgQ for status
# messages back to the main thread.
#########################################################################################
sub file_decoder_thread {
	while(1){
		# We get 4 things on the q per file...isbroken, tmpfilename, truefilename, decodedir
		my $isbroken= $decQ->dequeue;
		($isbroken =~ /^quit now$/) and last;	# Time to shut down
		my $tmpfilename = $decQ->dequeue;
		my $truefilename = $decQ->dequeue;
		my $decodedir = $decQ->dequeue;

		if(!$isbroken or $keepbrokenbin){
			$decMsgQ->enqueue("Starting decode of $truefilename");

			# Do the decode and confirm that it worked...
			# TODO: Make the debug file configurable and system flexible
			my $kb = '';
			$keepbrokenbin and $kb = '-d';	# If keeping broken, pass -d (desparate mode) to uudeview
			my $rc = system("$uudeview -i -a $kb -q \"$tmpfilename\" -p \"$decodedir\">> $DECODE_DBG_FILE 2>&1");
			$rc and $isbroken = 1;	# If decode failed, file is broken

			if($rc){		# Problem with the decode
				$decMsgQ->enqueue("FAILED decode of $tmpfilename (see $DECODE_DBG_FILE for details)");
			}
			else{
				$decMsgQ->enqueue("Completed decode of " . $truefilename);
			}
		}

		# Decide if we need to keep or delete the temp .parts file
		if($keepparts or ($isbroken and $keepbroken)){
			my $brokemsg = $isbroken ? ' broken' : '';
			$decMsgQ->enqueue("Keeping$brokemsg file segments in $tmpfilename (--keepparts given)");
			# TODO: rename to .broken
		}
		else {
			(unlink $tmpfilename) or $decMsgQ->enqueue("Error removing $tmpfilename from disk");
		}

	}
}
#########################################################################################
# Does multiplexed comms receiving
#########################################################################################
sub doReceiverPart {
	my ($rin, $ein) = ('', '');
	my ($rout, $eout);

	foreach my $i (1..$connct){
		next unless ($conn[$i-1]->{'sock'});
		vec($rin, fileno($conn[$i-1]->{'sock'}), 1) = 1;
		vec($ein, fileno($conn[$i-1]->{'sock'}), 1) = 1;
	}

	my $nfound = select($rout=$rin, undef, $eout=$ein, 0.25);  
	#statMsg("DEBUG: after select (nfound = $nfound)");

	foreach my $i (1..$connct){
		my $conn = $conn[$i-1];

		# TODO: Create a way to disable reconnection (when we don't want to do it)
		if(!$conn->{'sock'}){
			doReconnectLogicPart($i-1);
			#statMsg("DEBUG: skipping cuz sock closed");
			next;
		}
		
		if(vec($rout, fileno($conn->{'sock'}),1) == 1){
			my $recvret = recv $conn->{'sock'}, my $buff, $recv_chunksize, undef;

			#statMsg(sprintf("DEBUG: bstatus = %s", $conn->{'bstatus'}));
			#statMsg("DEBUG: buff just got: $buff");

			if(!defined($recvret) or !length($buff)){
				# TODO: Determine how to gracefully handle the crap we've already downloaded 
				close($conn->{'sock'});
				$conn->{'sock'} = undef;
				$conn->{'sleep start'} = time;
				statMsg(sprintf("* Remote disconnect on connection #%d", $i));
				drawStatusMsgs();
			}
			
			$conn->{'buff'} .= $buff;

			if(not ($conn->{'bstatus'} =~ /starting/)){ # only bump these up if we're not starting...
				$conn->{'segbytes'} += length($buff);
				$conn->{'filebytes'} += length($buff);
				$totals{'total bytes'} += length($buff);
			}

			$conn->{'last data'} = time;

			# Spool all lines from the buffer into the output file.
			while(1){
				my $ind1 = index $conn->{'buff'}, "\r\n";
				last unless $ind1 >= 0;
				my $line = substr $conn->{'buff'}, 0, $ind1+2, '';

				if($conn->{'bstatus'} =~ /starting/){

					#statMsg(sprintf("DEBUG: buffer has %d lines", scalar split /\r\n/s, $conn->{'buff'}));
					
					my ($mcode, $msize, $mbody, $mid) = split /\s+/, $line;
					#statMsg("DEBUG: msize for this line is $msize");

					# We're just starting, need to slurp up 222 (or other) response
					if($line =~ /^2\d\d\s.*\r\n/s){
						# Bad case where server sends a 5xx message after a 2xx (222)
						if(!$msize and ($conn->{'buff'} =~ /^5\d\d /)){
							# Handle this error condition (display message to user)
							my $errline = $conn->{'buff'};
							$errline =~ s/\r\n.*//s;
							statMsg(sprintf("Conn. %d: Server returned error: %s", $i, $errline));
						}
						else{
							#$conn[$i-1]->{'buff'} =~ s/2\d\d\s.*\r\n//;
							$conn->{'segbytes'} = length($conn->{'buff'});
						}
						$conn->{'bstatus'} = 'running';
					}
					else{ # This is an error condition -- often when the server can't find a segment
						$line =~ s/\r\n$//;
						statMsg( sprintf("Conn. %d FAILED to fetch part #%d (%s)", $i, 
										$conn->{'segnum'}+1, $line));
						drawStatusMsgs();
						$conn->{'bstatus'} = 'finished';  # Flag BODY segment as finished
						$conn->{'isbroken'} = 1;


						# Ok, so now that a segment fetch FAILED, we need to determine how to continue...
						# We will look at the keep variables to determine how to continue...
						# If keepbroken or keepbrokenbin are set, we will keep downloading parts...otherwise we will bump
						# up the segnum so that we skip all remaining segments (if any)
						
						if($keepbroken or $keepbrokenbin){		# If we shound continue downloading this broken file
							# Subtract the size of the current segment from the totals
							# (for this file and for the grand totals)
							my $failedsegsize = @{$conn->{'file'}->{'segments'}}[$conn->{'segnum'}]->{'size'};
							$totals{'total size'} -= $failedsegsize ;
							$conn->{'file'}->{'totalsize'} -= $failedsegsize;
						}
						else{
							statMsg(sprintf("Conn. %d aborting file (failed to fetch segment #%d)", 
									$i, $conn->{'segnum'}+1));
							
							# Adjust totals due to skipping failed file
							$totals{'total file ct'}--;
							$totals{'total bytes'} -= $conn->{'filebytes'}; # Remove bytes downloaded
							$totals{'total size'} -= $conn->{'file'}->{'totalsize'}; # Remove file bytes from job total 


							$conn->{'segnum'} = scalar @{$conn->{'file'}->{'segments'}} - 1;
							close $conn->{'tmpfile'};
							unlink $conn->{'tmpfilename'};
							$conn->{'file'} = undef;
						}
					}
					next;
				}

				# Try and detect the "real" filename
				if(not $conn->{'truefname'}){
					my $tfn = getTrueFilename($line);
					if($tfn){
						$conn->{'truefname'} = $tfn;
						statMsg("Conn. $i: Found true filename: $tfn");

						my $tfndisk = $tfn;			# Find out where it's going on disk...
						if(defined($dlpath)){
							$tfndisk = $dlpath . $tfn;
						}
						elsif(defined($dlrelative)){
							$tfndisk = $conn->{'file'}->{'nzb path'} . $tfn;
						}
						if(-e $tfndisk){
							if(!$overwritefiles){
								# We can't just close and delete, because there will likely still be 
								# data waiting in the receive buffer.  As such, we have to set a flag
								# to indicate that the file already exists and should be skipped...
								# This is perhaps a bit silly -- we have to finish slurping in the
								# BODY part before we can start working on the next file...
								statMsg("File already exists on disk (skipping after segment completes)");
								$conn->{'skipping'} = 1;
							}
						}
					}
				}
				
				if($line =~ /^\.\r\n/){		# detect end of BODY..
					$conn->{'bstatus'} = 'finished';
					if($conn->{'skipping'}){

						$totals{'total file ct'}--;
						$totals{'total bytes'} -= $conn->{'filebytes'}; # Remove bytes downloaded
						$totals{'total size'} -= $conn->{'file'}->{'totalsize'}; # Remove file bytes from job total 

						close $conn->{'tmpfile'};
						unlink $conn->{'tmpfilename'};
						$conn->{'file'} = undef;
						$conn->{'skipping'} = undef;	# no longer skipping (for next time)
					}
					last;
				}
				else{
					$line =~ s/^\.\././o;
					#statMsg("DEBUG: line is $line");
					print {$conn->{'tmpfile'}} $line;
				}
			}
		}
		drawScreenAndHandleKeys();
		doThrottling();
	}

	if($nfound == 0){	# No active connections, need to repaint anyway
		drawScreenAndHandleKeys();
		doThrottling();
	}
}


#########################################################################################
# Handles reconnection logic
#########################################################################################
sub doReconnectLogicPart {
	my $i = shift;
	my $conn = $conn[$i];

	my $remain = $reconndur - (time - $conn->{'sleep start'});
	if($remain > 0){	# still sleeping
		return;
	}
	my $iaddr = inet_aton($server) || die "Error resolving host: $server";
	my $paddr = sockaddr_in($port, $iaddr);

	statMsg(sprintf("Connection #%d attempting reconnect to %s:%d...", $i+1, $server, $port));
	($conn->{'sock'}, my $line) = createSingleConnection($i, $paddr, 1);

	my $msg = sprintf("Connection #%d reestablished.", $i+1);
	$user and $msg .= "..performing login";
	statMsg($msg);
	drawStatusMsgs();

	if($user){	#need to authenticate...
		doSingleLogin($i, 1);
		statMsg(sprintf("Login on connection #%d complete.", $i+1));
	}

	$conn->{'sleep start'} = undef;
	# These two lines reset our state so that we restart the segment we were on
	# prior to the disconnect.  Sure, a bit convoluted, but it's used elsewhere.
	$conn->{'bstatus'} = 'finished';
	$conn->{'segnum'}--;
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
				my $truefilename = $conn[$i-1]->{'truefname'};
				my $isbroken = $conn->{'isbroken'};
				my $outdir = "./";	# default to current dir
				if(defined($dlpath)){
					$outdir = $dlpath;
				}
				elsif(defined($dlrelative)){
					$outdir = $file->{'nzb path'};	# extract to same dir as nzb file
				}

				cursorPos(5, 6+(3*($i-1)));
				my $len = pc("File finished! Sending details to decoder queue...", 'bold white');
				print ' ' x ($wchar-$len-6);
				statMsg("Conn. $i: Finished downloading " . $conn[$i-1]->{'file'}->{'name'});
				# Queue the items to the decoding thread
				$decQ->enqueue($isbroken, $tmpfilename, $truefilename, $outdir);

				drawStatusMsgs();

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
		@{$conn->{'bwstarttime'}} = Time::HiRes::gettimeofday();

		# Create temp filename and open
		my $tmpfile = 'nzbperl.tmp' . time . '.' . $i . '.parts';
		if(defined($dlpath)){		# stick in dlpath if given
			$tmpfile = $dlpath . $tmpfile;
		}
		elsif(defined($dlrelative)){ # otherwise stick in relative dir to nzbfile
			$tmpfile = $file->{'nzb path'} . $tmpfile;
		}
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

	foreach my $i (1..$connct){
		my $iaddr = inet_aton($server) || die "Error resolving host: $server";
		my $paddr = sockaddr_in($port, $iaddr);
		($conn[$i-1]->{'sock'}, my $line) = createSingleConnection($i-1, $paddr);
	}
	return 1;
}

#########################################################################################
# Connects to an NNTP server and attempts to read the greet string line.
# Returns the socket and the greet line.
#########################################################################################
sub createSingleConnection {
	my ($i, $paddr, $silent) = @_;
	my $sock;
	socket($sock, PF_INET, SOCK_STREAM, getprotobyname('tcp')) || die "Error creating socket: $!";
	not $silent and printf("Attempting connection #%d to %s...", $i+1, $server);
	connect($sock, $paddr) || die "Error connecting: $!";
	not $silent and pc("success!\n", 'bold white');
	my $line = blockReadLine($sock);	# read server connection/response string
	not $line =~ /^(200|201)/ and die "Unexpected server response: $line" . "Expected 200 or 201.\n";
	return ($sock, $line);
}

#########################################################################################
# Attempts to perform a login on each connection
#########################################################################################
sub doLogins {
	foreach my $i (1..$connct){
		doSingleLogin($i-1);
	}
	return 1;
}

#########################################################################################
# Logs in a single connection.  Pass in the connection index.
#########################################################################################
sub doSingleLogin {
	my ($i, $silent) = @_;
	my $conn = $conn[$i];
	my $sock = $conn[$i]->{'sock'};
	not $silent and printf("Attempting login on connection #%d...", $i+1);
	send $sock, "AUTHINFO USER $user\r\n", undef;
	my $line = blockReadLine($sock);
	if($line =~ /^381/){
		send $sock, "AUTHINFO PASS $pw\r\n", undef;
		$line = blockReadLine($sock);
		$line =~ s/\r\n//;
		(not $line =~ /^281/) and not $silent and print ">FAILED<\n* Authentication to server failed: ($line)\n" and exit(0);
		not $silent and print "success!\n";
	}
	elsif($line =~ /^281/){ # not sure if this happens, but this means no pw needed I guess
		not $silent and print "no password needed, success!\n";
	}
	else {
		not $silent and print "server returned: $line\n";
		die ">LOGIN FAILED<\n";
	}
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
{
  my @old_speeds;
  sub getETA {
	  my ($h, $m, $s);
	  my $curspeed = getCurrentSpeed(1) || 0; # in bytes/sec
	  
	  if (push(@old_speeds, $curspeed) > 20) { # keep the last 20 measurements
		 shift(@old_speeds);
	  }
	  
	  my $avgspeed = 0;
	  foreach my $i (@old_speeds) {
		 $avgspeed += $i;
	  }
	  $avgspeed /= scalar(@old_speeds);
	  if ($avgspeed == 0) {
		 return "??:??:??";
	  }
	  
	  my $remainbytes = $totals{'total size'} - $totals{'total bytes'};
	  my $etasec = $remainbytes / $avgspeed;
	  $h = int($etasec/(60*60));
	  $m = int(($etasec-(60*60*$h))/60);
	  $s = $etasec-(60*60*$h)-(60*$m);
	  if($h > 240){	# likely bogus...just punt
		 return "??:??:??";
	  }
	  return sprintf("%.2d:%.2d:%.2d", $h, $m, $s);
	}
}

#########################################################################################
# Checks the last paint time and updates the screen if necessary.  Also checks for 
# keyboard keys.
#########################################################################################
sub drawScreenAndHandleKeys {
	if($showinghelpscreen){
		cursorPos(40, 14);
		pc("ETA: " . getETA(), 'bold green');
		pc(")", 'bold white');
		cursorPos(0, (1+5+(3*$connct)+8));
	}
	elsif((Time::HiRes::tv_interval(\@lastdrawtime) > 0.5) or # Refresh screen every 0.5sec max
		($decMsgQ->pending > 0)){							  # or we got status messages from decoder thread

		($wchar, $hchar, $wpixels, $hpixels) = GetTerminalSize();
		if($oldwchar != $wchar){
			$oldwchar and statMsg("Terminal was resized (new width = $wchar), redrawing");
			clearScreen();
			drawBorder();
		}
		$oldwchar = $wchar;
		@lastdrawtime = Time::HiRes::gettimeofday();
		drawHeader();
		drawConnInfos();
		drawStatusMsgs();

		cursorPos(0, (1+5+(3*$connct)+8));
		pc("'?' for help> ", 'bold white');
	}
	my $char;
	while (defined ($char = getch()) ) {	# have a key
		$char =~ s/[\r\n]//;
		handleKey($char);
	}
}
#########################################################################################
# getch -- gets a key in nonblocking mode
#########################################################################################
sub getch {
	$daemon and return;
	ReadMode ('cbreak');
	my $char;
	$char = ReadKey(-1);
	ReadMode ('normal');                  # restore normal tty settings
	return $char;
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
		if($sleepdur > 1.0){		# cap at 1 second sleep time, which is rediculously long anyway
			$sleepdur = 1.0;
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
# Trim the middle out of a string to shorten it to a target length
#########################################################################################
sub trimString {
	my $string = shift;
	my $target_len = shift;

	my $len = length($string);

	if($target_len >= $len || $target_len < 5) {
		return $string;
	}
	my $chop = $len - $target_len + 3; # 3 for the ...
	substr($string, ($len - $chop) / 2, $chop) = "...";
	return $string;
}

#########################################################################################
# Handles a keypress
#########################################################################################
sub handleKey {

	if($showinghelpscreen){
		$showinghelpscreen = 0;
		clearScreen();
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
		if($targkBps){
			$targkBps++;
			statMsg("Nudging bandwidth setting up to " . $targkBps . "kBps");
			drawStatusMsgs();
			updateBWStartPts();
		}
	}
	elsif($key =~ /-/){
		if(!$targkBps){ # Set to unlimited
			$targkBps = int(getCurrentSpeed(1)/1024)-1;
			statMsg("Nudging bandwidth setting down to " . $targkBps . "kBps");
			
		}
		elsif($targkBps > 1){ # Bottom out at 1
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
	cursorPos(2, 1);
	pc("nzbperl v.$version", 'bold red');
	pc(" :: ", 'bold white');
	#pc("by jason plumb ", 'bold red');
	#pc("and ", 'bold white');
	pc("noisybox.net", 'bold red');

	my $len = 0;
	cursorPos(2, 3);
	$len += pc("Files remaining: ", 'bold white');
	$len += pc($totals{'total file ct'}-$totals{'finished files'}, 'bold green');
	$len += pc(" of ", 'white');
	$len += pc($totals{'total file ct'}, 'bold green');
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
	pc((' ' x ($wchar-$len-4)), 'white');

	cursorPos(2, 2);
	$len = pc("Current speed: ", 'bold white');
	$len += pc(getCurrentSpeed() . "Bps", 'bold green');
	$len += pc(" (", 'bold blue');
	$len += pc("target", 'white');
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
	pc((' ' x ($wchar-$len-4)), 'white');
}


#########################################################################################
# Draws statuses for all individual connections
#########################################################################################
sub drawConnInfos(){
	my $startrow = 6;
	my $len;
	foreach my $i(1..$connct){
		my $conn = $conn[$i-1];

		cursorPos(2, $startrow+(3*($i-1)));

		if(!$conn->{'sock'}){	# connection closed
			$len = pc(sprintf("%d: ", $i), 'bold white');
			$len += pc("Connection is closed", 'bold red');
			if($conn->{'sleep start'}){	# will be a reconnect
				my $remain = $reconndur - (time - $conn->{'sleep start'});
				$len += pc(sprintf(" (reconnect in %s)", hrtv($remain)), 'bold yellow');
			}
			pc((' ' x ($wchar-$len-4)), 'white');

			cursorPos(2, $startrow+(3*($i-1))+1);
			pc((' ' x ($wchar-4)), 'white');
			next;
		}

		if(not $conn->{'file'}){
			if(scalar(@queuefileset) == 0){
				# This connection has no more work to do...
				$len = pc(sprintf("%d: Nothing left to do...", $i), 'bold cyan');
				pc((' ' x ($wchar-$len-4)), 'white');
				cursorPos(2, $startrow+(3*($i-1))+1);
				$len = pc("   <waiting for others to finish>", 'bold cyan');
				pc((' ' x ($wchar-$len-4)), 'white');
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

		$len = pc(sprintf("%d: Downloading: ", $i), 'bold white');
		my $fn = $file->{'name'};
		if( length($fn) + $len > $wchar-4){
			$fn = substr($fn, 0, $wchar-4-$len);
		}
		$len += pc($fn, 'white');
		if($len < $wchar-4){
			pc(' ' x ($wchar-$len-4), 'white');
		}

		cursorPos(2, $startrow+(3*($i-1))+1);
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
			$len += pc( sprintf("%2d", ($filebytesread/$filesize)*100) . "%", 'bold yellow');
		}
		else{
			$len += pc("??%", 'bold yellow');
		}
		$len += pc(' ' x (7-length(hrsv($filebytesread))) . "[", 'bold white');
		#$len += pc(sprintf("%5s", hrsv($filebytesread)), 'bold green');
		$len += pc(hrsv($filebytesread), 'bold green');
		$len += pc("/", 'bold white');
		$len += pc(hrsv($filesize), 'bold green');
		$len += pc("]", 'bold white');
		$len += pc("  [part ", 'bold white');
		$len += pc($segnum, 'bold cyan');
		$len += pc("/", 'bold white');
		$len += pc($segct, 'bold cyan');
		$len += pc(" ", '');
		$len += pc(sprintf("%4s", hrsv($segbytesread)), 'bold cyan');
		$len += pc("/", 'bold white');
		$len += pc(hrsv($cursegsize), 'bold cyan');
		$len += pc("]", 'bold white');

		pc((' ' x ($wchar - $len - 4)), 'white');

	}

}

#########################################################################################
sub drawStatusMsgs {
	# TODO:  Consider saving state about status messages -- could save cycles by not
	#        automatically drawing every time.
	$showinghelpscreen and return;
	my $row = 3*$connct + 6 + 1;
	my $statuslimit = 6;	# number of lines to show.

	# Pull any decode messages from the queue and append them
	# This might not be the *best* place for this...
	while($decMsgQ->pending > 0){
		statMsg($decMsgQ->dequeue);
	}

	# Trim status messages to size
	while( scalar(@statusmsgs) > $statuslimit){
		shift @statusmsgs;
	}
	foreach my $line (@statusmsgs){
		cursorPos(2, $row);
		if(length($line) > $wchar-4){
			$line = substr($line, 0, $wchar-4);	# Clip line
		}
		else{
			$line .= (' ' x ($wchar-4-length($line)));
		}
		pc($line, 'white');
		$row++;
	}
	cursorPos(0, (1+5+(3*$connct)+8));
	pc("'?' for help> ", 'bold white');
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
	cursorPos(0, $ypos);
	pc('+' . ('-' x ($wchar-2)) . '+', 'bold white');
}
sub drawVLine {
	my $xpos = shift;
	my $height = shift;
	not $height and $height = (1+5+(3*$connct)+7);
	foreach(0..$height){
		cursorPos($xpos, $_);
		pc('|', 'bold white');
	}
}

#########################################################################################
# helper for printing in color (or not)
#########################################################################################
sub pc {
	my ($string, $colstr) = @_;
	$daemon and return length($string);
	if($usecolor){
		print colored ($string, $colstr);
	}
	else{
		print $string;
	}
	return length($string);
}

sub clearScreen {
	!$daemon and 
		$terminal->Tputs('cl', 1, *STDOUT);			# clears screen
}

#########################################################################################
# Positions the cursor at x,y.  Looks at $daemon first.
#########################################################################################
sub cursorPos {
	my ($x, $y) = @_;
	!$daemon and 
		$terminal->Tgoto('cm', $x, $y, *STDOUT);
}
#########################################################################################
# Adds a status message with timestamp
#########################################################################################
sub statMsg {
	my $str = shift;
	my @t = localtime;
	my $msg = sprintf("%0.2d:%0.2d - %s", $t[2], $t[1], $str);
	push @statusmsgs, $msg;
	if($logfile){
		open LOGFH, ">>" . $logfile or 
				(push @statusmsgs, sprintf("%0.2d:%0.2d:%0.2d - Error writing to log file  %s", $logfile) and return 1); 
		print LOGFH sprintf("%d-%02d-%02d %s\n", $t[5]+1900, $t[4]+1, $t[3], $msg);
		close LOGFH;
	}
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
		not $sock and print "(already closed)\n" and next;
		send $sock, "QUIT\r\n", undef;
		my $line = blockReadLine($sock);
		$line =~ /^205/ and print "closed gracefully!";
		print "\n";
		close $sock;
		$conn[$i-1]->{'sock'} = undef;
	}
}


#########################################################################################
# human readable time value (from seconds)
#########################################################################################
sub hrtv {
	my $sec = shift;
	if($sec < 60){
		return $sec . "s";
	}
	my $h = int($sec/(60*60));
	my $m = int(($sec - ($h*60*60))/60.0);
	my $s = $sec - ($h*60*60) - ($m*60);
	if($h){
		return sprintf("%02d:%02d:%02d", $h, $m, $s);
	}
	else{
		return sprintf("%02d:%02d", $m, $s);
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
	return sprintf("%0.2f", $size);
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

	my $nzbfilename = shift;
	my $nzbdir = derivePath($nzbfilename);
	my $parser = new XML::DOM::Parser;
	my @fileset;
	print "Loading and parsing nzb file: " . $nzbfilename . "\n";
	my $nzbdoc = $parser->parsefile($nzbfilename);
	my $files = $nzbdoc->getElementsByTagName("file");
	my $totalsegct = 0;
	foreach my $i (0..$files->getLength()-1){
		my $fileNode = $files->item($i);
		my $subj = $fileNode->getAttributes()->getNamedItem('subject');
		my $postdate = $fileNode->getAttributes()->getNamedItem('date');

		my %file;
		$file{'nzb path'} = $nzbdir;
		$file{'name'} = $subj->getValue();
		$file{'date'} = $postdate->getValue();

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

			my $segNumber = $seg->getAttributes()->getNamedItem('number')->getValue();

			$seghash{'msgid'} = $seg->getFirstChild()->getNodeValue();
			$seghash{'size'} = $size;
			$seghash{'number'} = $segNumber;

			$segments[$segNumber-1] = \%seghash;
		}
		$totalsegct += scalar @segments;
		$file{'segments'} = \@segments;

		push @fileset, \%file;
	}
	$nzbdoc->dispose;

	print "Loaded $totalsegct total segments for " . $files->getLength() . " file(s).\n";
	return @fileset;
}

#########################################################################################
# Filters out files if there is a filter regex, and skips over files from --skip <n>
#########################################################################################
sub regexAndSkipping {
	if(defined($filterregex)){
		print "Filtering files on regular expression...\n";
		my $orgsize = scalar @fileset;
		my @nset;
		while(scalar(@fileset) > 0){
			my $f = shift @fileset;
			if( $f->{'name'} =~ /$filterregex/){
				push @nset, $f;
			}
		}
		if(scalar @nset < 1){
			pc("\nWhoops:  Filter removed all files (nothing left)...aborting!\n\n", 'bold yellow') and exit 0;
		}
		printf("Kept %d of %d files (filtered %d)\n", scalar(@nset), $orgsize, $orgsize-scalar(@nset));
		@fileset = @nset;
	}

	if($skipfilect){
		if($skipfilect >= scalar @fileset){
			pc("\nWhoops:  --skip $skipfilect would skip ALL " . scalar @fileset . 
					" files...aborting!\n\n", 'bold yellow') and exit 0;
		}
		print "Removing $skipfilect files from nzb set (--skip $skipfilect)\n";
		while($skipfilect > 0){
			shift @fileset;
			$skipfilect--;
		}
	}
	
	return @fileset;
}

#########################################################################################
# Sorts files in the fileset based on the name
#########################################################################################
sub sortFilesBySubject {
	if(!$nosort){
		print "Sorting files by filename (subject)...";
		@fileset = 
			sort {
				$a->{'name'} cmp $b->{'name'};
			} @fileset;
		print "finished.\n";
			
	}
}
#########################################################################################
# Derives a path from a filename (passed on commandline).
# The result isn't necessarily absolute, can be relative
#########################################################################################
sub derivePath {
	my $filename = shift;
	if($filename =~ /\//){		# then it has path information, likely not windows compat
		$filename =~ s/(^.*\/).*/\1/;
		return $filename;
	}
	return './';
}
#########################################################################################
# Main entry point for NZB file sanity checking
#########################################################################################
sub doNZBSanityChecks(){
	print "Analyzing sanity of NZB file segment completeness...\n";
	@suspectFileInd = getSuspectFileIndexes();
	my $badfilect = scalar @suspectFileInd;
	not $badfilect and pc("All files pass segment size sanity checks!  Swell.\n", 'bold green') and return;

	SMENUDONE:
	while(1){
		pc(sprintf("There are %d of %d files that may have missing or broken segments.\n", $badfilect, scalar @fileset), 'bold yellow');
		pc("It is likely that these files will be unusable if downloaded.\n", 'bold yellow');
		($dropbad or $insane) and return;	# User selection not needed.
		print "\n How do you want to proceed?\n\n";
		print " k)eep everything and try all files anyway (--insane)\n";
		print " d)rop files suspected broken (--dropbad)\n";
		print " v)iew gory details about broken segments\n";
		print " q)uit now\n";
		print "\n -> ";
		while(1){
			my $char;
			if(defined ($char = getch()) ) {	# have a key
				print "\n";
				if($char =~ /q/){
					exit 1;
				}
				elsif($char =~ /k/){
					print "Setting --insane option...\n";
					$insane = 1;
					last SMENUDONE;
				}
				elsif($char =~ /d/){
					print "Setting --dropbad option...\n";
					$dropbad = 1;
					last SMENUDONE;
				}
				elsif($char =~ /v/){
					showSuspectDetails(@suspectFileInd);
				}
				last;
			}
			else{
				select undef, undef, undef, 0.1;
			}
		}
	}
}

#########################################################################################
# Shows details about suspect files...
#########################################################################################
sub showSuspectDetails {
	my @susFileInd = @_;
	foreach my $fileind (1..scalar @susFileInd){
		my $file = @fileset[$susFileInd[$fileind-1]];
		my $avgsize = avgFilesize($file);
		print "------------------------------------------------------\n";
		printf(" * File: %s\n", $file->{'name'});
		printf("   Posted on: %s (%d days ago)\n", 
				scalar localtime $file->{'date'},
				(time - $file->{'date'})/(60*60*24) );
		printf("   Adjusted average part size = %d bytes\n", $avgsize);
		my @sids = getSuspectSegmentIndexes($file, $avgsize);
		foreach my $si (@sids){
			my $seg = @{$file->{'segments'}}[$si];
			my $percdiff = 100;
			$avgsize and $percdiff = 100*(abs($seg->{'size'} - $avgsize)/$avgsize);
			printf("      Part %d : %d bytes (%.2f%% error from average)\n",
					$si+1, $seg->{'size'}, $percdiff);
		}
	}
	print "------------------------------------------------------\n";
}

#########################################################################################
# Looks at the fileset and returns an array of file indexes that are suspect
#########################################################################################
sub getSuspectFileIndexes {
	my @ret;
	foreach my $fileind (1..scalar @fileset){
		my $file = @fileset[$fileind-1];
		my $avg = avgFilesize($file);
		#printf("File has average size = %d\n", $avg);
		my $segoffct = 0;

		my @suspectSegInd = getSuspectSegmentIndexes($file, $avg);
		if(scalar @suspectSegInd){
			push @ret, $fileind-1;
		}
	}
	return @ret;
}

#########################################################################################
sub getSuspectSegmentIndexes {
	my $MAX_OFF_PERC = 25;		# Percentage of segment size error/diff to trigger invalid
	my ($file, $avg) = @_;
	my @ret;
	#print "DEBUG: File subject = " . $file->{'name'} . "\n";
	#print "DEBUG: It has " . (scalar @{$file->{'segments'}}) . " parts\n";
	foreach my $i (1..(scalar @{$file->{'segments'}}-1)){  # Last segment is allowed to slide...
		my $seg = @{$file->{'segments'}}[$i-1];
		my $percdiff = 100;
		$avg and $percdiff = 100*(abs($seg->{'size'} - $avg)/$avg);
		#printf("   seg $i of %d is %0.2f off avg [%d versus %d (avg)]\n", scalar @{$file->{'segments'}}, $percdiff, $seg->{'size'}, $avg);
		if($percdiff > $MAX_OFF_PERC){
			push @ret, $i-1;
		}
	}
	return @ret;
}

#########################################################################################
sub dropSuspectFiles(){ my @newset; my $dropct = 0; foreach my $i (0..scalar @fileset-1){
		if($i == $suspectFileInd[0]){
			my $ind = shift @suspectFileInd;
			my $file = @fileset[$ind];
			printf("Dropping [%s] from filset (suspect)\n", $file->{'name'});
			$dropct++;
			next;
		}
		push @newset, shift @fileset;
	}
	@fileset = @newset;
	pc(sprintf("Dropped %d suspect files from NZB (%d files remain)\n", $dropct, scalar @fileset), 'bold yellow');
	print " -> short delay (for user review)";
	foreach(5,4,3,2,1){
		print "...$_";
		sleep 1;
	}
	print "...let's go!\n";
}

#########################################################################################
# Not a true average, but an average of all segments except the last one...
# ...unless there's only one segment, in which case it's the segment size.
#########################################################################################
sub avgFilesize {
	my $file = shift;
	my @segs = @{$file->{'segments'}};
	return $segs[0]->{'size'} unless scalar @segs > 1;
	my ($sum, $ct) = (0, 0);
	foreach my $i (1..scalar(@segs)){
		my $seg = @segs[$i-1];
		last unless $i < scalar(@segs);
		$ct++;
		$sum += $seg->{'size'};
	}
	return $sum*1.0/($ct*1.0);
}

#########################################################################################
# Parse command line options and assign sane globals etc.
#########################################################################################
sub handleCommandLineOptions {
	my @saveargs = @ARGV;
	GetOptions(%optionsmap);
	if(-e $configfile){
		readConfigFileOptions();
	}
	else {
		print "Config file $configfile does not exist.  Skipping.\n";
	}
	
	@ARGV = @saveargs;	# restore
	GetOptions(%optionsmap);
	if($help){
		showUsage();
		exit 1;
	}
	$nocolor and $usecolor = 0;

	if(not $ARGV[0]){		# No NZB file given?
		print "Missing nzb file.\n";
		showUsage(); 
		exit;
	}

	if(not length($server)){
		$server = $ENV{'NNTPSERVER'};
		not $server and die "Must provide --server or set \$NNTPSERVER environment\n";
	}
	if($server =~ /:\d+$/){
		$port = $server;
		$port =~ s/.*://;
		$server =~ s/:.*//;
	}
	# Make sure that dlpath ends with a slash
	$dlpath and (not ($dlpath =~ /\/$/)) and ($dlpath .= '/');

	if($dropbad and $insane){	# conflicting
		print "Error: --dropbad and --insane are conflicting (choose one)\n";
		showUsage();
		exit;
	}
	if($dlpath and $dlrelative){ # conflicting options
		print "Error: --dlrelative and --dlpath <dir> are conflicting (choose one)\n";
		showUsage();
		exit;
	}
}

# Helper to detect that uudeview is installed.  Always a good idea, ya'know, since we're
# dependant on it!
sub haveUUDeview {
	if(defined($uudeview)){	# Given on commandline or config file
		if(-e $uudeview){
			pc("uudeview found: $uudeview\n", "bold green");
			return 1;
		}
		pc("Warning: uudeview not found at location $uudeview\n", "bold yellow");
	}
	my @paths = split /:/, $ENV{'PATH'};	# path sep different on winderz?
	foreach my $p (@paths){
		$p =~ s/\/$//;
		$p = $p . "/uudeview";
		if(-e $p){
			pc("uudeview found: $p\n", "bold green");
			$uudeview = $p;
			return 1;
		}
	}
	pc("Error: uudeview not found in path...aborting!\n", "bold red");
	return 0;
}

sub readConfigFileOptions(){
	print "Reading config options from $configfile...\n";
	open CFG, "<$configfile" or die "Error opening $configfile for config options";
	my $line;
	my @opts;
	while($line = <CFG>){
		chomp $line;
		$line =~ s/^\s+//;
		$line =~ s/^-+//;	# In case dashes in config file
		next if $line =~ /^#/;
		next unless length($line);
		push @opts, "--$line";
	}
	close CFG;
	my @oldarg = @ARGV;
	@ARGV = @opts;
	GetOptions(%optionsmap);
	@ARGV = @oldarg;
}
#########################################################################################
# Checks for a newer version, disabled with --noupdate
#########################################################################################
sub checkForNewVersion {
	$noupdate and return;	# they don't want update checking
	print "Checking for availability of newer version...\n";
	eval "use LWP::Simple;";
	if($@){
		print "LWP::Simple is not installed, skipping up-to-date check.\n";
		return;
	}
	my $remote_ver = eval "get \"$UPDATE_URL\"";
	chomp $remote_ver;

	if($remote_ver eq $version){
		print "Look like you're running the most current version.  Good.\n";
	}
	else{
		pc("A newer version of nzbperl is available: ", 'bold red');
		pc('version ' . $remote_ver . "\n", 'bold white');
		pc("You should consider downloading it from ", 'bold white');
		pc("http://noisybox.net/computers/nzbperl/\n", 'bold yellow');
		pc("This delay is intentional: ");
		foreach(1..8){
			print "..." . (9-$_);
			sleep 1;
		}
	}

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
	clearScreen();
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

  Connected to $server:$port
  (Your download is still in progress:  
  
  [ Press any key to return to the main screen ]

EOL
;
	drawVLine(0, 17);
	drawVLine($wchar, 17);
	drawHLine(0);
	drawHLine(17);

	cursorPos(40, 14);
	pc("ETA: " . getETA(), 'bold green');
	pc(")", 'bold white');
	$showinghelpscreen = 1;
}

#########################################################################################
# Show program usage
#########################################################################################
sub showUsage {
print <<EOL

  nzbperl version $version -- usage:

  nzbperl <options> <file1.nzb> ... <file.nzb>

  where <options> are:

  --config <file>   : Use <file> for config options (default is ~/.nzbperlrc)
  --server <server> : Usenet server to use (defaults to NNTPSERVER env var)
                    : Port can also be specified with --server <server:port>
  --user <user>     : Username for server (blank of not needed)
  --pw <pass>       : Password for server (blank to prompt if --user given)
  --conn <n>        : Use <n> server connections (default = 2)
  --keepparts       : Keep all encoded parts files on disk after decoding
  --keepbroken      : Continue downloading files with broken/missing segments
                    : and leave the parts files on disk still encoded.
  --keepbrokenbin   : Decode and keep broken decoded files (binaries) on disk.
  --dlrelative      : Download and decode to the dir that the nzbfiles are in
                    : (default downloads to current directory)
  --dlpath <dir>    : Download and decode all files to <dir>  
                    : (default downloads to current dirctory)
  --redo            : Don't skip over existing downloads, do them again
  --insane          : Bypass NZB sanity checks completely
  --dropbad         : Auto-skip files in the NZBs with suspected broken parts
  --skip <n>        : Skip the first <n> files in the nzb (don't process)
  --med <kBps>      : Set "med" bandwidth to kBps (default is 95kBps)
  --low <kBps>      : Set "low" bandwidth to kBps (default is 35kBps)
  --speed <speed>   : Explicitly specify transfer bandwidth in kBps
  --log <file>      : Log status messages into <file> (default = none)
  --daemon          : Run in background as daemon (use log for status)
  --retrywait <n>   : Wait <n> seconds between reconnect tries (default = 300)
  --nosort          : Don't sort files by name before processing
  --filter <regex>  : Filter NZB contents on <regex> in subject line
  --uudeview <app>  : Specify full path to uudeview (default found in \$PATH)
  --nocolor         : Don't use color
  --noupdate        : Don't check for newer versions at startup
  --help            : Show this screen

  During runtime, press 'h' or '?' to see a list of key commands.

  nzbperl version $version, Copyright (C) 2004 Jason Plumb
  nzbperl comes with ABSOLUTELY NO WARRANTY; This is free software, and 
  you are welcome to redistribute it under certain conditions;  Please 
  see the source for additional details.

EOL
}

