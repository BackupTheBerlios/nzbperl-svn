#!/usr/bin/perl -w
#
# nzbperl.pl -- version 0.7.0-pre
# 
# for more information:
# http://noisybox.net/computers/nzbperl/ 
#
# note: the figlet font used in package separators is "standard.tlf"
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

# A couple of useful static imports
Nzb::Printer->import(qw(statMsg porlp));

# Begin global food chain
my $version = '0.7.0-pre';
my $ospeed = 9600; 
my $terminal = Tgetent Term::Cap { TERM => undef, OSPEED => $ospeed };
my $recv_chunksize = 5*1024;	# How big of chunks we read at once from a connection (this is pulled from ass)
my $UPDATE_URL = 'http://noisybox.net/computers/nzbperl/nzbperl_version.txt';

my $dispchunkct = 250;			# Number of data lines to read between screen updates.
my $targkBps = 0;
my ($medbw, $lowbw) = (95, 35);	# Defaults for low and medium speed settings.
my $sleepdur = 0;				# Used when throttling

# Make stdout not buffered.
my $old_fh = select(STDOUT);
$| = 1;
select($old_fh);

my $urunid = time;	#unique run id
my $quitnow = 0;
my $showinghelpscreen = 0;
my $usecolor = 1;
my $paused = 0;
my $displayMode = 0;

my ($oldwchar, $wchar, $oldhchar, $hchar, $wpixels, $hpixels) = (0);  	# holds screen size info
my $queue = Nzb::Queue->new();
# TODO: Make a stats package
my %totals;
my @statusmsgs;
my $lastDirCheckTime = 0;
my $lastDiskFullTime = undef;
my $lastDiskFreePerc = 0;
my %nzbfiles;	# the hash/queue of nzb files we're handling
# $nzbfiles{'files'}->{<filename>}->{'read'}     : 1 if we've parsed/loaded it
# $nzbfiles{'files'}->{<filename>}->{'finished'} : 1 if all files have been downloaded
my @lastdrawtime;
my @connections;
my ($decMsgQ, $decQ, @decThreads);
my $rc_sock = undef;
my @rc_clients;
my @dlstarttime;

my $conf = Nzb::Config->new();

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   _   _         _  
#  | \ | |  ____ | |__
#  |  \| | |_  / | '_ \
#  | |\  |  / /  | |_) |  main package
#  |_| \_| /___| |_.__/ 
#                      
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
package Nzb;

use strict;
use File::Basename;
use IO::File;
use IO::Select;
use XML::DOM;
use Time::HiRes;	# timer stuff
use Term::ReadKey;	# for no echo password reading
use Term::Cap;
use Data::Dumper;	# DEBUG
use Cwd;

Nzb::Printer->import(qw(statMsg porlp));

exit (main())  if $0 eq __FILE__; # Used to facilitate package bonding and unit testing
return 1;


# NZBPERL MAIN METHOD
sub main {

if(defined(my $errmsg = $conf->handleCommandLineOptions(@ARGV))){
	showUsage($errmsg); 
	exit 1;
}

if(not $conf->ipv6){
	use IO::Socket::INET;
}
if(not $conf->nocolor){
	use Term::ANSIColor;
}
if(not haveUUDeview()){	# Verify that uudeview is installed
	pc("* Please install and configure uudeview and try again.\n", "bold red");
	exit 1;
}
$conf->uudeview =~ m#^([\w\s\.\_\-\/\\]+)$# or die "Invalid characters in uudeview path.";

displayShortGPL();

checkForNewVersion();

if($conf->user and !$conf->pw){
	printf("Password for '%s': ", $conf->user);
	$conf->pw(readPassword());
}
if(defined($conf->proxy_user) and not defined($conf->proxy_passwd)){
	printf("SOCKS Password for '%s': ", $conf->proxy_user);
	$conf->proxy_passwd(readPassword());
}

while(scalar(@ARGV) > 0){
	parseAndEnqueueNzbFile(shift @ARGV);
}

%totals = (
	'total size' => $queue->computeTotalNzbSize(),
	'finished files' => 0,
	'total bytes' => 0,
	'total file ct' => $queue->pendingFileCount()
);
print "Looks like we've got " . $queue->pendingNzbFileCount() . " nzb files with " . 
	$queue->pendingFileCount() . " possible files ahead of us.\n";

startRemoteControl();

statMsg('Welcome -- nzbperl started!');

@lastdrawtime = Time::HiRes::gettimeofday();
@connections = createNNTPConnections();

if($conf->daemon){
	print "nzbperl is running in --daemon mode, output suppressed\n";
	print "Check log for additional details during run...\n";
	my $pid = fork;
	$pid and print "Daemon started [$pid]\n" and exit;
}

# Start up the decoding thread(s)...
if($conf->usingThreadedDecoding){
	$decMsgQ = Thread::Queue->new;	# For status msgs
	$decQ = Thread::Queue->new;

	foreach my $i (1..$conf->dthreadct){
		push @decThreads, threads->new(\&file_decoder_thread, $i);
	}
}

($wchar, $hchar, $wpixels, $hpixels) = GetTerminalSize();
clearScreen();
@dlstarttime = Time::HiRes::gettimeofday();
drawScreenAndHandleKeys(); # show the initial screen

while(1){

	doSegmentAssignments();
	doReceiverPart();
	decodeAnyFinishedFiles();

	queueNewNZBFilesFromDir();	# queue up new nzb files from dir (guards inside)

	dequeueNextNZBFileIfNecessary();
	doRemoteControls(); 

	$quitnow and last;
	if($queue->isEmpty() and defined($conf->forever)){
		select undef, undef, undef, 0.25;
		next;
	}
	$queue->isEmpty() and last;
}

cursorPos(0, $hchar);
pc("All downloads complete!\n", 'bold white');
cursorPos(0, $hchar);

if($quitnow){ # Do some cleanups after forced quit
	removeAnyPartsFiles();
}

if(defined($rc_sock)){ 	# Clean up remote control socket if necessary
	close($rc_sock);
	$rc_sock = undef;
}

disconnectAll();
pc("Waiting for file decoding thread(s) to terminate...\n", 'bold white');
foreach my $i (1..$conf->dthreadct){
	# Send a quit now message for each decoder thread.
	$conf->usingThreadedDecoding and $decQ->enqueue('quit now');
}
foreach my $i (0 .. $conf->dthreadct-1){
	# Now join on every decoder thread, waiting for all to finish
	$conf->usingThreadedDecoding and $decThreads[$i]->join;
}
pc("Thanks for using ", 'bold yellow');
pc("nzbperl", 'bold red');
pc("! Enjoy!\n\n", 'bold yellow');
}# END OF NZBPERL MAIN

#########################################################################################
# no_more_work_to_do - returns 1 if there is more work to do, 0 otherwise.  Used to 
# detect when the main loop body should terminate
#########################################################################################
sub no_more_work_to_do {
	foreach my $conn (@connections){
		if($conn->{file}){
			return 0;
		}
	}
	return $queue->isEmpty();	# no more files in queue, all done
}
#########################################################################################
# This is the thread that does file decoding
# It uses two queues for communication -- decQ for files to decode, decMsgQ for status
# messages back to the main thread.
#########################################################################################
sub file_decoder_thread {
	my $threadNum = shift;

	my ($nzbfile, $isbroken, $islastonnzb, $tmpfilename, $truefilename, $decodedir);
	my $prefixMsg = ($conf->dthreadct > 1) ? "Decoder #$threadNum:" : '';

	while(1){
		# We get 6 things on the q per file...
		# nzbpath, $nzbfile, isbroken, tmpfilename, truefilename, decodedir
		$nzbfile = $decQ->dequeue;

		last unless defined($nzbfile);
		($nzbfile =~ /^quit now$/) and last;	# Time to shut down

		$isbroken= $decQ->dequeue;
		$islastonnzb = $decQ->dequeue;
		$tmpfilename = $decQ->dequeue;
		$decodedir = $decQ->dequeue;
		$truefilename = $decQ->dequeue;

		my $nzbpath = dirname($nzbfile);
		doUUDeViewFile($nzbfile, $isbroken, $islastonnzb, 
						$tmpfilename, $decodedir, $truefilename, $prefixMsg);
	}
}
#########################################################################################
# Does multiplexed comms receiving
#########################################################################################
sub doReceiverPart {
	my $readSelect = IO::Select->new();
	my $errSelect = IO::Select->new();

	foreach my $conn (@connections){
		$conn->addSocketToSelect($readSelect);
		$conn->addSocketToSelect($errSelect);
	}

	# If there are no active connections, we need to do a little sleep to prevent maxing out cpu.
	# the select->can_read call passes right through if it has no handles.
	(not $readSelect->count()) and select undef, undef, undef, 0.1;

	#my @errs = $select->has_error(0);
	#if(scalar @errs > 0){
	#}

	#my @ready = $select->can_read(0.25);	# 250ms worst case block time

	my ($rset, $wset, $eset) = IO::Select->select($readSelect, undef, $errSelect, 0.25);

	foreach my $conn (@connections){

		my $recvct = $conn->readDataIfAvailable(@{$rset}, @{$eset});

		#if ($canread) {
		#	my ($recvret, $buff);
		#	if (ref($conn->{'sock'}) eq "IO::Socket::SSL") {
		#	    $recvret = $conn->{'sock'}->sysread($buff, $recv_chunksize);
		#	} 
		#	else {
		#	    $recvret = recv($conn->{'sock'}, $buff, $recv_chunksize, 0);
		#		if(defined($recvret)){
		#			$recvret = length $buff;
		#		}
		#		else{
		#			$recvret = -1;
		#		}
		#	}
		#
		#	if(($recvret < 0) or !length($buff)){
		#		# TODO: Determine how to gracefully handle the crap we've already downloaded 
		#		if (ref($conn->{'sock'}) eq "IO::Socket::SSL") {
		#			$conn->{'sock'}->shutdown( 2 );
		#			$conn->{'sock'}->close( SSL_no_shutdown => 1 );
		#		} 
		#		else {
		#			$conn->{'sock'}->close;
		#		}
		#		$conn->{'sock'} = undef;
		#		$conn->{'sleep start'} = time;
		#		statMsg(sprintf("* Remote disconnect on connection #%d", $conn->number));
		#		drawStatusMsgs();
		#		$conn->{'buff'} = '';
		#		next;
		#	}
			
		#	if(not connIsStartingSeg($conn)){ # only bump these up if we're not starting...
		#		$conn->{'segbytes'} += length($buff);
		#		$conn->{'filebytes'} += length($buff);
		#		$totals{'total bytes'} += length($buff);
		#	}
#
#			$conn->{'last data'} = time;
#
		drawScreenAndHandleKeys();
		doThrottling();
	}

	if (scalar @{$rset} == 0) {		# If we didn't do it above, we gotta do it here
		drawScreenAndHandleKeys();
		doThrottling();
	}
}

#########################################################################################
# Gets a list of "completed" (or possibly skipped) files from the queue and either
# decodes them or does cleanup.
#########################################################################################
sub decodeAnyFinishedFiles {
	#Note: the call to popCompletedFiles walks the nzb file tree (each nzb each file), which could introduce
	#extra (certainly unnecessary) cpu overhead.  If this becomes an issue, we probably want to
	#check for file completion only after segment completion and then somehow pop the file off the
	#queue into a new bucket.
	foreach my $file ($queue->popCompletedFiles()){
		$totals{'finished files'}++;
		if($file->skipped() or (!$conf->overwritefiles and $file->existsOnDisk())){
			cleanupAfterSkipping($file);
		}
		else{
			my $mergedFile = mergePartsForFile($file);
			doDecodeOrQueueCompletedFile($file, $mergedFile);
		}
	}
}

#########################################################################################
# Take an Nzb::File object and merge all available segments on disk.  Return the new
# filename (full path).  Note: No validation.  If segments were missed/dropped/skipped
# or whatever, this will still merge what's there.
#########################################################################################
sub mergePartsForFile {
	my $file = shift;
	$file->isBroken() and return;
	statMsg("Merging segments for " . $file->trueFilename);
	my $target = sprintf('%s/nzbperl.%d.f%d.part', $conf->getDestDirForFile($file), $urunid, $file->uid);
	open TARGET, '>' . $target or 
		statMsg("**ERROR: Can't open target for tmp file merge: $!") and return undef;

	foreach my $part (getPartsForFile($file)){
		open SRC, $part or statMsg("**ERROR opening $part for merge! $!") and next;
		my ($buff, $rc);
		while(($rc = read SRC, $buff, 5*1024*1024) != 0){		# read 5M at a time
			syswrite TARGET, $buff, $rc;
		}
		close SRC;
		unlink $part;
	}
	close TARGET;
	return $target;
}

#########################################################################################
sub cleanupAfterSkipping {
	my $file = shift;
	statMsg(sprintf("Deleting parts for skipped file %s", $file->trueFilename()));
	foreach my $part (getPartsForFile($file)){
		unlink $part;
	}
}
#########################################################################################
sub getPartsForFile {
	my $file = shift;
	my $dir = $conf->getDestDirForFile($file);
	opendir TMPDIR, $dir or die "Error opening temp dir while trying to discover parts: $!";
	my $mask = sprintf('nzbperl\.%d\.f%d\.s\d\d\d\d\d\d\.part', $urunid, $file->uid);
	my @parts = grep /$mask/, readdir TMPDIR;
	map { $_ =~ s/^/$dir\//;} @parts;	# prepend each guy with full dir
	@parts = sort {	$a cmp $b } @parts;
	closedir TMPDIR;
	return @parts;
}

#########################################################################################
# This is a cleanup routine that deletes *any* parts files associated with any 
# part of the queue.  It walks the queue tree and removes guys that exist.
#########################################################################################
sub removeAnyPartsFiles {
	foreach my $nzb (@{$queue->nzbFiles()}){
		foreach my $file (@{$nzb->files()}){
			my $dir = $conf->getDestDirForFile($file);
			foreach my $seg ($file->segments()){
				my $tmpfile = sprintf('%s/nzbperl.%d.f%d.s%06d.part',
						$dir, $urunid, $file->uid, $seg->number);
				if(-e $tmpfile){
					pc("Closing and deleting " . $tmpfile . "...\n", 'bold white');
					unlink $tmpfile;
				}
			}
		}
	}
}

#########################################################################################
# makes the given dowload dir if necessary
#########################################################################################
sub makeTargetDirIfNecessary {
	my $targdir = shift;
	if( not -d ($targdir) and defined($conf->dlpath) and 
		(defined($conf->dlcreate) or defined($conf->dlcreategrp))){
		if(!mkdir($targdir)){
			statMsg("ERROR: Could not create $targdir: $!");
		}
	}
}

#########################################################################################
# startSegmentOnConnection - Handles an input line when a segment is just starting
# on a connection.  This looks into detecting missing segments and handles server 
# responses that mean various things.
#########################################################################################
#sub startSegmentOnConnection {
#	my ($conn, $line) = @_;
#	my ($mcode, $msize, $mbody, $mid) = split /\s+/, $line;
#
#	# We're just starting, need to slurp up 222 (or other) response
#	if($line =~ /^2\d\d\s.*\r\n/s){
#		# Bad case where server sends a 5xx message after a 2xx (222)
#		if(!$msize and ($conn->{'buff'} =~ /^5\d\d /)){
#			# Handle this error condition (display message to user)
#			my $errline = $conn->{'buff'};
#			$errline =~ s/\r\n.*//s;
#			statMsg(sprintf("Conn. %d: Server returned error: %s", $conn->number, $errline));
#		}
#		else{
#			$conn->{'segbytes'} = length($conn->{'buff'});
#		}
#		$conn->{'bstatus'} = 'running';
#	}
#	else{ # This is an error condition -- often when the server can't find a segment
#		$line =~ s/\r\n$//;
#		statMsg( sprintf("Conn. %d FAILED to fetch part #%d (%s)", $conn->number, 
#						$conn->{'segnum'}+1, $line));
#		drawStatusMsgs();
#		$conn->{'bstatus'} = 'finished';  # Flag BODY segment as finished
#		$conn->{'isbroken'} = 1;
#
#
#		# Ok, so now that a segment fetch FAILED, we need to determine how to continue...
#		# We will look at the keep variables to determine how to continue...
#		# If keepbroken or keepbrokenbin are set, we will keep downloading parts...otherwise we will bump
#		# up the segnum so that we skip all remaining segments (if any)
#		
#		if($keepbroken or $keepbrokenbin){		# If we shound continue downloading this broken file
#			# Subtract the size of the current segment from the totals
#			# (for this file and for the grand totals)
#			my $failedsegsize = @{$conn->{'file'}->{'segments'}}[$conn->{'segnum'}]->{'size'};
#			$totals{'total size'} -= $failedsegsize ;
#			$conn->{'file'}->{'totalsize'} -= $failedsegsize;
#		}
#		else{
#			statMsg(sprintf("Conn. %d aborting file (failed to fetch segment #%d)", 
#					$conn->number, $conn->{'segnum'}+1));
#			
#			# Adjust totals due to skipping failed file
#			$totals{'total file ct'}--;
#			$totals{'total bytes'} -= $conn->{'filebytes'}; # Remove bytes downloaded
#			$totals{'total size'} -= $conn->{'file'}->{'totalsize'}; # Remove file bytes from job total 
#
#
#			$conn->{'segnum'} = scalar @{$conn->{'file'}->{'segments'}} - 1;
#			undef $conn->{'tmpfile'};	# causes a close
#			unlink $conn->{'tmpfilename'};
#			$conn->{'file'} = undef;
#		}
#	}
#}

#########################################################################################
# Handles reconnection logic
#########################################################################################
sub doReconnectLogicPart {
	my $conn = shift;
	my $forceNow = shift; # can be specified to force a reconnect right now

	if(not $forceNow){
		my $remain = $conf->reconndur - (time - $conn->{'sleep start'});
		if($remain > 0){	# still sleeping
			return;
		}
	}
	#my $iaddr = inet_aton($server) || die "Error resolving host: $server";

	statMsg(sprintf("Connection #%d attempting reconnect to %s:%d...", $conn->number, $conf->server, $conf->port));

	die "DEBUG: I need to fix this......sorry";
	my $line;
	# TODO: Fix/implement this -- commented out only for quick compilation!!!!
	#($conn->{'sock'}, my $line) = createSingleConnection("$server:$port", 1);

	if(!$conn->{'sock'}){		# couldn't reconnect
		statMsg($line);
		$conn->{'sleep start'} = time;	# reset reconnect timer
		return;
	}

	my $msg = sprintf('Connection #%d reestablished.', $conn->number);
	$conf->user and $msg .= "..performing login";
	statMsg($msg);
	drawStatusMsgs();

	if($conf->user){	#need to authenticate...
		$conn->performLogin();
		statMsg(sprintf("Login on connection #%d complete.", $conn->number));
	}

	$conn->{'sleep start'} = undef;
	# These two lines reset our state so that we restart the segment we were on
	# prior to the disconnect.  Sure, a bit convoluted, but it's used elsewhere.
	$conn->{'bstatus'} = 'finished';
	defined($conn->{'segnum'}) and $conn->{'segnum'}-- 
		unless $conn->{'segnum'} < 0;;
}

#########################################################################################
# Handles segments and detects when we're done with a file
#########################################################################################
#sub doBodyRequests {
#	foreach my $conn (@connections){
#		next unless $conn->hasSegment;		# Ignore conn for now if it doesn't have a segment
#
#		if($conn->{'segnum'} < 0){
#			next unless $conn->{'sock'}; # no socket, perhaps waiting for reconnect
#			$conn->{'segnum'} = 0;
#			my $seg = @{$file->{'segments'}}[0];
#			#$conn->{'seg'} = $seg;
#			my $msgid = $seg->{'msgid'};
#
#			sockSend($conn->{'sock'}, 'BODY <' . $msgid . ">\r\n");
#
#			$conn->{'bstatus'} = 'starting';
#			$conn->{'segbytes'} = 0;
#		}
#		elsif($conn->{'bstatus'} =~ /finished/){ # finished a segment
#			$conn->{'segnum'}++;
#
#			if($conn->{'segnum'} >= scalar @{$file->{'segments'}}){ # All segments for this file exhausted.
#
#				cursorPos(5, 6+(3*($conn->number - 1)));
#				my $len = pc("File finished! Sending details to decoder queue...", 'bold white');
#				print ' ' x ($wchar-$len-6);
#				statMsg(sprintf("Conn. %d: Finished downloading %s", $conn->number, $conn->{'file'}->name));
#
#				doDecodeOrQueueCompletedFile($conn);
#				drawStatusMsgs();
#
#				$totals{'finished files'}++;
#				$conn->{'file'} = undef;
#				#$conn->{'seg'} = undef;
#
#			}
#			else {
#				next unless $conn->{'sock'}; # no socket, perhaps waiting for reconnect
#				my $segnum = $conn->{'segnum'};
#				my $seg = @{$file->{'segments'}}[$segnum];
#				my $msgid = $seg->{'msgid'};
#
#				sockSend($conn->{'sock'}, 'BODY <' . $msgid . ">\r\n");

#				$conn->{'bstatus'} = 'starting';
#				$conn->{'segbytes'} = 0;
#			}
#		}
#	}
#}
#########################################################################################
# doStartFileDecoding - initiates or performs a decode for a completed file.
# If dthreadct == 0, this will decode in place, otherwise it just queues the request
# to decode to the decoder thread.
#########################################################################################
sub doDecodeOrQueueCompletedFile {
	my ($file, $mergedFilename) = @_;
	return unless !$file->isBroken() or $conf->keepbrokenbin;
	my $outdir = $conf->getDestDirForFile($file);

	my $isbroken = $file->isBroken();
	my $islastonnzb = 0;	# DEBUG TODO FIX THIS!  Hacked for now to make things go.

	if($conf->usingThreadedDecoding()){
		# Queue the items to the decoding thread
		$decQ->enqueue($file->parentNzb()->nzbFile, $isbroken, $islastonnzb, $mergedFilename, 
						$outdir, $file->trueFilename);
	}
	else{
		doUUDeViewFile($file->parentNzb()->nzbFile, $isbroken, $islastonnzb, $mergedFilename, $outdir, $file->trueFilename);
	}
}

#########################################################################################
# Decodes a file to disk and handles cleanup (deleting/keeping parts)
#########################################################################################
sub doUUDeViewFile {
	my ($nzbfile, $isbroken, $islastonnzb, $tmpfilename, 
		$decodedir, $truefilename, $prefixMsg) = @_;

	my $nzbpath = dirname($nzbfile);
	$prefixMsg = '' unless defined($prefixMsg);
	$prefixMsg =~ s/\s+$//;
	length($prefixMsg) and $prefixMsg .= ' ';

	# Do the decode and confirm that it worked...
	if(!$isbroken or $conf->keepbrokenbin){
		statOrQ($prefixMsg . "Starting decode of $truefilename");
		my $kb = '';
		$conf->keepbrokenbin and $kb = '-d';	# If keeping broken, pass -d (desparate mode) to uudeview
		my $decodelogpart = '';
		my $qopts = '-q';
		if(defined($conf->DECODE_DBG_FILE)){
			$decodelogpart = " >> " . $conf->DECODE_DBG_FILE;
			$qopts = '-n';
		}
		else{
			$decodelogpart = " > /dev/null";
		}

		my $rc = system($conf->uudeview . " -i -a $kb $qopts \"$tmpfilename\" -p \"$decodedir\"$decodelogpart 2>&1");
		$rc and $isbroken = 1;	# If decode failed, file is broken

		if($rc){		# Problem with the decode
			if(defined($conf->DECODE_DBG_FILE)){
				statOrQ($prefixMsg . "FAILED decode of $tmpfilename (see " . $conf->DECODE_DBG_FILE . " for details)");
			}
			else{
				statOrQ($prefixMsg . "FAILED decode of $tmpfilename");
				statOrQ("Consider using --decodelog <file> to troubleshoot.");
			}
		}
		else{
			statOrQ($prefixMsg . "Completed decode of " . $truefilename);
		}
	}

	# Decide if we need to keep or delete the temp .parts file
	if($conf->keepparts or ($isbroken and $conf->keepbroken)){
		my $brokemsg = $isbroken ? ' broken' : '';
		statOrQ("Keeping$brokemsg file segments in $tmpfilename (--keepparts given)");
		# TODO: rename to .broken
	}
	else {
		unlink($tmpfilename) or statOrQ("Error removing $tmpfilename from disk: $!");
	}

	runPostDecodeProgram($tmpfilename, $decodedir, $truefilename, $isbroken);
	$islastonnzb and (runPostNzbDecodeProgram($nzbpath, $nzbfile, $decodedir, $truefilename));
}
#########################################################################################
# runPostDecodeProgram -- Possibly runs an external program after a file has been
# decoded (regardless of success).
#########################################################################################
sub runPostDecodeProgram {
	my ($tmpfilename, $decodedir, $truefilename, $isbroken) = @_;
	return unless defined($conf->postDecProg);

	$truefilename = $decodedir . 
		(($decodedir =~ /\/$/) ? '' : '/') . $truefilename;
	
	runProgWithEnvParams($conf->postDecProg, 'post-decoding',
		NZBP_FILE => $truefilename, NZBP_TEMPFILE => $tmpfilename,
		 NZBP_ISBROKEN => $isbroken);
}

#########################################################################################
# runPostNzbDecodeProgram -- Possibly runs external prog when nzb is completed.
#########################################################################################
sub runPostNzbDecodeProgram {
	my ($nzbpath, $nzbfile, $decodedir, $truefilename) = @_;
	return unless defined($conf->postNzbProg);	#option not specified	
	runProgWithEnvParams($conf->postNzbProg, 'post-nzb', 
		NZBP_NZBDIR => $nzbpath, NZBP_NZBFILE => $nzbfile,
		NZBP_DECODEDIR => $decodedir, NZBP_LASTFILE => $truefilename);
}
#########################################################################################
# Runs a program with environment vars prepended to the commandline as parameters.
# This is used by the post decoder program runner and the post nzb program runner.
#########################################################################################
sub runProgWithEnvParams {
	my ($prog, $desc, %env) = @_;
	my $cmd = '';
	# This is a little strange...but showing the env vars onto the command is 
	# the only way I could find to pass environments from a perl thread to 
	# an external prog.  I wish there was a better way (like using $ENV, but
	# that fails)
	foreach my $k (keys %env){
		my $envitem = $env{$k};
		$envitem =~ s/"/\\"/g;		# escape double quotes
		$envitem =~ s/`/\\`/g;		# escape backticks (evil)
		$cmd .= sprintf("export %s=\"%s\"; ", $k, $envitem);
	}
	$cmd .= $prog;
	statMsg("Running $desc program : $prog");
	system($cmd);
	statMsg("Finished running $desc program.");
	drawStatusMsgs();
}
#########################################################################################
# When a connection needs a new segment to work on, this will pull one off the queue
# and assign it one.
#########################################################################################
sub doSegmentAssignments {
	$paused and return;					# don't assign if we're paused
	foreach my $conn (@connections){
		if(not $conn->hasSegment()){
			my $seg = $queue->getNextSegment();
			defined($seg) and $conn->startNewSegment($seg);
		}
	}
}

#########################################################################################
# Returns 1 if the param to prevent disk filling was set and we're within the threshhold
#########################################################################################
sub hitDiskSpaceLimit {
	my ($nzb, $file) = @_;
	return 0 unless defined $conf->diskfree;

	# Only check freespace every 15 seconds
	if(defined($lastDiskFullTime)){
		return 1 unless (time - $lastDiskFullTime) > 15;
	}

	my $freeperc = getFreeDiskPercentage($conf->getDestDirForFile($file));
	if($freeperc <= $conf->diskfree){
		if(not defined($lastDiskFullTime)){ # the first time we detect free space is out
			statMsg(sprintf('Warning: Download disk has less than %d%% free.', $conf->diskfree));
			statMsg("Waiting for free space before continuing downloading.");
		}
		$lastDiskFullTime = time;
		return 1;
	}
	$lastDiskFullTime = undef;
	return 0;
}

#########################################################################################
# Tries to get the free disk percentage on the provided path
#########################################################################################
sub getFreeDiskPercentage {
	my $path = shift;
	my @reslines = `df '$path'`;
	my $line = pop @reslines;
	chomp $line;
	# Are all dfs created equal???  If not, we could use col headers?
	my ($fs, $size, $used, $avail, $dfperc, $mount) = split /\s+/, $line; 
	$dfperc =~ s/%//;
	$lastDiskFreePerc = 100-$dfperc;
	return $lastDiskFreePerc;
}

#########################################################################################
# Decides if its time to do the next nzb file...which is when the queue is empty and there
# is at least 1 idle connection;
#########################################################################################
sub dequeueNextNZBFileIfNecessary {
	
	return unless $queue->isEmpty;		# still have queued files

	foreach my $conn (@connections){
		if(not $conn->{'file'}){	# the connection is idle
			my ($newQueuedCt, $dequeuedNewFile, $reconnAttempts) = (0,0,0);

			$newQueuedCt = queueNewNZBFilesFromDir(1);	# force a dircheck first
			$dequeuedNewFile = dequeueNextNZBFile();
			if($dequeuedNewFile){
				$reconnAttempts = reconnectAllDisconnectedNow();
			}

			if($newQueuedCt or $dequeuedNewFile or $reconnAttempts){
				drawStatusMsgs();
			}
			return;
		}
	}
}

#########################################################################################
# Forces an immediate reconnect on all not connected connections.
# Returns number of connections that had reconnect *attempts* (not necessarily the 
# number that were actually reconnected)
#########################################################################################
sub reconnectAllDisconnectedNow {
	my $retCt = 0;
	foreach my $conn (@connections){
		if(not defined($conn->{'sock'})){
			doReconnectLogicPart($conn, 1);
			$retCt++;
		}
	}
	return $retCt;
}
#########################################################################################
# Pulls out the next nzb file in queue, parses it, and then add its files/parts to 
# @queuefileset.
#########################################################################################
sub dequeueNextNZBFile {
	my @keys = keys %{$nzbfiles{'files'}};
	foreach my $key (@keys){
		if(not $nzbfiles{'files'}->{$key}->{'read'}){
			statMsg("Moving to next nzb file in queue: $key");
			my @newset = parseNZB($conf->queuedir . '/' . $key, 1);
			if(!defined($newset[0])){
				statMsg("Warning: no new files loaded from queued nzb file");
				return 0;
			}
			$queue->queue_files(@newset);
			statMsg("Loaded " . scalar(@newset) . " new files to download from nzb file: $key");
			$totals{'total file ct'} += scalar @newset;
			$totals{'total size'} += $queue->computeTotalNzbSize();
			$nzbfiles{'files'}->{$key}->{'read'} = 1;
			return 1;
		}
	}
	return 0;
}

#########################################################################################
# queues new nzb files from the queue dir if they exist and adds them to the hash/queue
# of all nzb files we're processing.  Returns the number of files dequeued.
#########################################################################################
sub queueNewNZBFilesFromDir {
	my $forcecheck = shift;
	return 0 unless ($conf->queuedir and $queue->isEmpty);
	return 0 unless $forcecheck or (time - $lastDirCheckTime > 15);	# don't check more than once every 15 seconds
	$lastDirCheckTime = time;

	my $retCt = 0;
	opendir(QDIR, $conf->queuedir);
	my @candidates = grep(/\.nzb$/, readdir(QDIR));
	foreach my $file (@candidates){

		if(not $queue->haveAlreadyQueued($file)){
			statMsg("Queueing new nzb file found on disk: $file");
			# TODO: Fix me
		}
		
		#if( !defined($nzbfiles{'files'}->{$file})){	# not queued yet
		#	statMsg("Queueing new nzb file found on disk: $file");
		#	$nzbfiles{'files'}->{$file}->{'read'} = 0;
		#	$retCt++;
		#}
	}
	closedir(QDIR);
	return $retCt;
}

#########################################################################################
# Start up the remote control(s)
#########################################################################################
sub startRemoteControl {
	return unless defined($conf->rcport);	# nuthin to do

	eval "use XML::Simple;";
	($@) and die "ERROR: XML::Simple required if using remote control...Please install it.";

	$rc_sock = createRCMasterSocket();
	
	print "Remote control server socket created.\n";
}

#########################################################################################
# creates the remote control master port, using either ipv4 or ipv6
#########################################################################################
sub createRCMasterSocket {
	my $ret;

	my %opts = (Listen => 5, LocalAddr => 'localhost',
				LocalPort => $conf->rcport,
				Proto=>'tcp', Type => SOCK_STREAM, Reuse => 1); 
	if($conf->ipv6){
		$ret = IO::Socket::INET6->new( %opts ) or die "Error creating remote control socket: $!";
	}
	else{
		$ret = IO::Socket::INET->new( %opts ) or die "Error creating remote control socket: $!";
	}
	return $ret;
}

#########################################################################################
# Main loop entry point for handling remote control stuff
#########################################################################################
sub doRemoteControls {
	return unless defined($conf->rcport);	
	getNewRcClients();
	handleRcClients();
	my @tmprcc;
	foreach my $client (@rc_clients){				#clean up dropped clients
		if(defined($client->{'closenow'})){
			statMsg(sprintf("Remote control client from %s:%s disconnected.", $client->{'ip'}, $client->{'port'}));
			close $client->{'sock'};
		}
		else{
			push @tmprcc, $client;
		}
	}
	@rc_clients = @tmprcc;
}

#########################################################################################
# handleRcClients -- read and handle all remote commands from all clients
#########################################################################################
sub handleRcClients{
	for (my $i=0; $i < scalar @rc_clients; $i++){
		my $client = $rc_clients[$i];
		my $cmd = readRcClientCommand($client);
		defined($cmd) and handleRcClientCmd($client, $cmd);
	}
}
#########################################################################################
# handleRcClientCmd - Handle's an rc client command
#########################################################################################
sub handleRcClientCmd {
	my ($client, $cmdstr) = @_;
	my ($cmd, $params, $responsemsg) = ($cmdstr, $cmdstr);
	$cmd =~ s/\s+.*//;
	$params =~ s/^\w+\s+//;
	$params = '' unless $params ne $cmd;
	if($cmd =~ /ping/i){
		$responsemsg = sprintf("PONG! %s", $params);
	}
	elsif($cmd =~ /^quit/i){
		sendRemoteResponse($client, "Nice having ya.");
		$client->{'closenow'} = 1;
		return;
	}
	elsif($cmd =~ /^keys/i){
		my @keys = split //, $params;
		foreach my $key (@keys){
			handleKey($key);
		}
		$responsemsg = sprintf("Ok, processed %d keystrokes", scalar @keys);
	}
	elsif($cmd =~ /^summary/i){
		$responsemsg = generateRcSummary();
	}
	elsif($cmd =~/^speed/i){
		if($params =~ /\d+/){
			$targkBps = $params;
			$responsemsg = sprintf("Ok, set download speed to %dkBps", $params);
		}
		else{
			$responsemsg = "Error: please specify speed in kBps";
		}
	}
	elsif($cmd =~ /^diskfree/i){
		$params =~ s/%//;
		$conf->diskfree($params);
		$responsemsg = "Ok, set max disk free percentage to " . $conf->diskfree . "%";
	}
	elsif($cmd =~ /^enqueue/i){
		if(defined($nzbfiles{'files'}->{$params})){	# not queued yet
			$responsemsg = "Error: Refusing to queue file already queued ($params).";
		}
		elsif(not -e $params){
			$responsemsg = "Error: File does not exist ($params)";
		}
		else{
			$responsemsg = "Queueing new nzb file found on disk: $params";
			statMsg($responsemsg);
			$nzbfiles{'files'}->{$params}->{'read'} = 0;
		}
	}
	else{
		$responsemsg = "Sorry, command not understood.";
	}
	sendRemoteResponse($client, $responsemsg);
}
#########################################################################################
# sendRemoteResponse -- send a remote command response to the remote client.
#########################################################################################
sub sendRemoteResponse {
	my ($client, $msg) = @_;
	# simple protocol, eh?
	send $client->{'sock'}, sprintf("%d\r\n%s\r\n", length($msg)+2, $msg), 0;
}
#########################################################################################
# readRcClientCommand -- Attempts to read a command from the client socket
# returns the command or undef
#########################################################################################
sub readRcClientCommand {
	my $client = shift;
	my $buff = readNewRcClientSockData($client);
	if(defined($buff)){
		my $nlindex = index $client->{'data'}, "\r\n";
		if($nlindex >= 0){
			#get cmd and replace in client{data} with nothing
			my $cmd = substr $client->{'data'}, 0, $nlindex+2, '';	
			$cmd = trimWS($cmd);
			return $cmd;
		}
	}
	return undef;
}
#########################################################################################
# readNewRcClientSockData -- Pulls client data off the socket if there is any.
#########################################################################################
sub readNewRcClientSockData {
	my $client = shift;
	my $sock = $client->{'sock'};
	my $sockfn = fileno($sock);
	my ($rin, $win, $ein) = ('', '', '');
	my ($rout, $wout, $eout);
	vec($rin, $sockfn, 1) = 1;
	vec($win, $sockfn, 1) = 1;
	vec($ein, $sockfn, 1) = 1;
	my $nfound = select($rout=$rin, $wout=$win, $eout=$ein, 0);  
	return undef unless $nfound > 0;
	if(vec($rout, $sockfn,1) == 1){
		my $buff;
		recv($sock, $buff, $recv_chunksize, 0);
		if(not length($buff)){
			$client->{'closenow'} = 1;
			return undef;
		}
		$client->{'data'} .= $buff;
		return $buff;
	}
	if((vec($eout, $sockfn, 1) == 1) || (vec($wout, $sockfn, 1) != 1)){
		$client->{'closenow'} = 1;
	}
	return undef;
}

#########################################################################################
# Accepts new connections from clients and adds them to the list.
#########################################################################################
sub getNewRcClients {
	while(1){
		my ($rin,$rout) = ('','');
		vec($rin, fileno($rc_sock), 1)  = 1;
		my $nfound = select($rout=$rin, undef, undef, 0);  
		last unless ($nfound > 0);
		my $nclient;
		my $client_addr = accept($nclient, $rc_sock);
		#my $old = select($nclient);
		#$| = 1;	# make nonbuffered
		#select($old);
		my ($clientport, $clientippart) = sockaddr_in($client_addr);
		my $clientip = inet_ntoa($clientippart);
		statMsg("New remote control connection from " . $clientip . ":" . $clientport);
		send $nclient, "nzbperl version $version\r\n", 0;
		push @rc_clients, {'sock' => $nclient, 'ip' => $clientip, 'port' => $clientport};
	}
}

#########################################################################################
# generateRcSummary - generates a summary of information for a remote request for it.
#########################################################################################
sub generateRcSummary {
	my %s;
	$s{'connections'} = scalar @connections;
	my $tspeed = $targkBps ? hrsv($targkBps*1024) . "Bps" : "unlimited";
	$s{'speeds'} = {'current' => getCurrentSpeed(), 'target' => $tspeed, 'session' => getTotalSpeed()};
	$s{'completed'} = {'files' => $totals{'finished files'}, 'size' => hrsv($totals{'total bytes'})};
	$s{'completed'}->{'files'} = 0 unless $s{'completed'}->{'files'};
	$s{'remaining'} = {'files' => $totals{'total file ct'}-$totals{'finished files'},
						'time' => getETA(), 'size' => hrsv($totals{'total size'} - $totals{'total bytes'}),
						'queued_nzb_files' => $queue->pendingNzbFileCount()};
	my $summary = XML::Simple->new()->XMLout(\%s, rootname => 'summary', noattr => 1);
	$summary =~ s/\s+$//;
	return $summary ;
}
#########################################################################################
# Creates all connections and returns an array holding the connections
#########################################################################################
sub createNNTPConnections {
	my @connections;
	foreach my $i (1..$conf->connct){
		push @connections, createNewConnectionAndMaybeLogin($i, 1, $conf->server, $conf->port, $conf->user, $conf->pw);
	}
	return @connections;
}

sub createNewConnectionAndMaybeLogin {
	my ($i, $connectAndLogin, $server, $port, $user, $pw) = @_;
	my $conn = Nzb::Connection->new($i, $server, $port, $user, $pw);
	if($connectAndLogin){
		$conn->connect();
		if($user){
			$conn->performLogin() or die "Error authenticating to server.\nPlease check the user/pass info and try again.";
		}
	}
	return $conn;
}

#########################################################################################
# Computes and returns the total speed for this session.
#########################################################################################
sub getTotalSpeed {
	my $runtime = Time::HiRes::tv_interval(\@dlstarttime);
	return uc(hrsv($totals{'total bytes'}/$runtime)) . 'Bps';
}

#########################################################################################
# Looks at all the current connections and calculates a "current" speed
#########################################################################################
sub getCurrentSpeed {
	my $suppresshsrv = shift;
	my $sumbps = 0;
	map { $sumbps += $_->currentSpeed(); } @connections;
	$suppresshsrv and return $sumbps;
	return uc(hrsv($sumbps)) . 'Bps';
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
	$conf->daemon and return;	# don't draw screen when in daemon mode...RC keys handled elsewhere
	if((Time::HiRes::tv_interval(\@lastdrawtime) > 0.5) or # Refresh screen every 0.5sec max
		($conf->usingThreadedDecoding and $decMsgQ->pending > 0)){  # or we got status messages from decoder thread

		if($showinghelpscreen){
			showHelpScreenEta();
		}
		else{

			($wchar, $hchar, $wpixels, $hpixels) = GetTerminalSize();
			if($oldwchar != $wchar){
				$oldwchar and statMsg("Terminal was resized (now $wchar x $hchar), redrawing");
				clearScreen();
				drawBorder();
			}
			$oldwchar = $wchar;
			drawHeader();
			drawMainContentSection();
			drawStatusMsgs();

			cursorPos(0, $hchar);
			pc("'?' for help> ", 'bold white');
		}
		@lastdrawtime = Time::HiRes::gettimeofday();
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
	$conf->daemon and return;
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

	# Force sleep time when paused...downside is that we max bandwidth until
	# all segments finish...upside is that we pause faster and are more responsive.
	$paused and ($sleepdur = 0.1);	

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
	}
	elsif($key =~ /1/){
		$targkBps = $lowbw;
		statMsg("Setting bandwidth to low value ($lowbw" . "kBps)");
		drawStatusMsgs();
	}
	elsif($key =~ /2/){
		$targkBps = $medbw;
		statMsg("Setting bandwidth to medium value ($medbw" . "kBps)");
		drawStatusMsgs();
	}
	elsif($key =~ /3/){
		$targkBps = 0;	# set to high 
		statMsg("Setting bandwidth to maximum (unlimited)");
		drawStatusMsgs();
	}
	elsif($key =~ /h/ or $key =~ /\?/){
		statMsg("Displaying help screen at user's request");
		showHelpScreen();
	}
	elsif($key =~ /c/){
		$usecolor = $usecolor ^ 0x01; #invert value
	}
	elsif($key =~ / /){
		((++$displayMode) < 2) or $displayMode = 0;
		statMsg("Changing to next display mode.");
	}
	elsif($key =~ /p/){
		$paused = $paused ^ 0x01; #invert value
		if($paused){
			statMsg("Starting <pause> (connections close when segments finish)");
		}
		else{
			statMsg("Unpausing....connections will reconnect now.");
		}
	}
	elsif($key =~ /s/){
		my $skipFile = getSkipFile();
		statMsg("User requested <s>kip for " . $skipFile->name);
		$skipFile->skipped(1);
	}
	elsif($key =~ /a/){	# add a connection
		statMsg("Adding new connection at user's request.");
		push @connections, createNewConnectionAndMaybeLogin(scalar(@connections)+1, 0, $conf->server, $conf->port, $conf->user, $conf->pw);
	}
	elsif($key =~ /d/){ # drop a connection
		if(scalar(@connections) > 1){
			dropLastConnection();
		}
		else{
			statMsg("Refusing to reduce connections below 1.");
		}
	}
	elsif($key =~ /\+/){
		if($targkBps){
			$targkBps++;
			statMsg("Nudging bandwidth setting up to " . $targkBps . "kBps");
			drawStatusMsgs();
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
	}
	else {
		statMsg("Unknown key: $key (try 'h' for help)");
	}
}

sub dropLastConnection {
	my $c = scalar @connections;
	statMsg(sprintf("Dropping connection %d at user's request.", $c));
	# TODO: Clean up .part file that connection may be writing
	# TODO: Requeue part in progress
	my $conn = pop @connections;
	$conn->closeConnection();
	undef($conn);
}

#########################################################################################
# Draws the header that contains summary info etc.
#########################################################################################
sub drawHeader(){
	cursorPos(2, 1);
	my $len = 0;
	$len += pc("nzbperl v.$version", 'bold red');
	$len += pc(" :: ", 'bold white');
	$len += pc("noisybox.net", 'bold red');
	my $queuedCount = $queue->pendingNzbFileCount();
	if($queuedCount > 0){
		$len += pc("  [", 'bold blue');
		$len += pc(sprintf("+%d nzb files queued", $queuedCount), 'bold cyan');
		$len += pc("]", 'bold blue');
	}
	if(scalar @rc_clients > 0){
		$len += pc("  [", 'bold blue');
		$len += pc(sprintf("%d remotes", scalar @rc_clients), 'bold cyan');
		$len += pc("]", 'bold blue');
	}
	pc((' ' x ($wchar-$len-4)), 'white');

	cursorPos(2, 3);
	$len += pc("Files remaining: ", 'bold white');
	$len += pc($totals{'total file ct'} - $totals{'finished files'}, 'bold green');
	$len += pc(" of ", 'white');
	$len += pc($totals{'total file ct'}, 'bold green');
	my $dlperc = $totals{'total size'} == 0 ? 0 : int(100.0*$totals{'total bytes'} / $totals{'total size'});
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
	if($paused){
		$len = pc("<<<", 'bold yellow');
		$len += pc(" PAUSED ", 'bold cyan');
		$len += pc(">>>", 'bold yellow');
		$len += pc(" (press <p> to unpause)", 'white');
	}
	else{
		$len = pc("Current speed: ", 'bold white');
		$len += pc(getCurrentSpeed(), 'bold green');
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
		$len += pc(getTotalSpeed(), 'bold green');
	}
	pc((' ' x ($wchar-$len-4)), 'white');
}

#########################################################################################
# Draws the main "body" of the screen that contains file (or possibly other stuff)
#########################################################################################
sub drawMainContentSection {
	if($displayMode == 0){
		return drawFileInfos();
	}
	return drawConnInfos();
}

#########################################################################################
# Draw progress/status info for each file that's active
#########################################################################################
sub drawFileInfos {
	my $i = 0;
	my $row = 5;
	my @allfiles = getRemainingFiles();

	while($row < $hchar-14){
		my $file = shift @allfiles;
		$row += drawSingleFileInfo($file, $row);
	}

	if(scalar @allfiles > 0){
		cursorPos(2, $row);
		pc(sprintf("... + %d more ...", scalar @allfiles), 'bold yellow');
	}
}


#########################################################################################
# Draws info for a single file.  May be a blank line if input file is undef, may be
# 2 lines if the file is in progress, or may be a single line.
#########################################################################################
sub drawSingleFileInfo {
	my ($file, $row) = @_;

	cursorPos(1, $row);
	not defined $file and pc(' ' x ($wchar-2), 'white') and return 1;

	my $fn = $file->name;
	if( length($fn) > $wchar-5){	# Truncate displayed filename if too long
		$fn = substr($fn, 0, $wchar - 5);
	}

	my $len = pc(" * " . $fn, $paused ? 'yellow' : 'bold white');
	pc((' ' x ($wchar-$len-2)), 'white');		# blank out to end of line

	return 1 unless $file->bytesDownloaded > 0; # in progress

	cursorPos(1, ++$row);
	my $perc = $file->percentComplete();

	my $pbarw = int(25*($file->percentComplete()/100));
	($pbarw > 25) and $pbarw = 25;	# cap progress bar length
	if($conf->noansi) {
		$len = pc("   |", 'bold white');
		if($pbarw){
			$len += pc('#' x ($pbarw-1), 'bold white');
			$len += pc('#', 'bold red');
		}
		$len += pc('-' x (25-$pbarw), 'white');
		$len += pc("| ", 'bold white');
	}
	else {
		pc("\x1B(0" . "   [", 'bold white');
		$len = 4;	# escape key screws up length counting
		if($pbarw){
			$len += pc('a' x ($pbarw-1), 'bold white');
			$len += pc('a', 'bold red');
		}
		$len += pc('q' x (25-$pbarw), 'white');
		pc("] " . "\x1B(B", 'bold white');
		$len += 2;

	}
	$len += pc(int($perc) . '%', 'bold yellow');
	$len += pc(' ' x (7-length(hrsv($file->bytesDownloaded))) . "[", 'bold white');
	$len += pc(hrsv($file->bytesDownloaded), 'bold green');
	$len += pc("/", 'bold white');
	$len += pc(hrsv($file->totalSize), 'bold green');
	$len += pc("]", 'bold white');
	$len += pc("  [parts ", 'bold white');
	$len += pc($file->completedSegmentCount(), 'bold cyan');
	$len += pc("/", 'bold white');
	$len += pc($file->segmentCount(), 'bold cyan');
	$len += pc("]", 'bold white');
	pc(' ' x ($wchar-$len-2), 'white');		# blank out to end of line

	return 2;
}

sub getSkipFile {
	return $queue->nzbFiles()->[0]->files()->[0];
}
sub getRemainingFiles {
	my @ret;
	map { 
		map {
			push @ret, @{$_->files()}
		} $_
	} @{$queue->nzbFiles};
	return @ret;
}
sub getFilesInProgress {
	my %ret;
	foreach my $conn (@connections){
		my $file = $conn->currentFile();
		next unless defined $file;
		$ret{$file->uid} = $file;
	}
	return values %ret;
}

#########################################################################################
# Draws statuses for all individual connections
#########################################################################################
sub drawConnInfos(){
	my $i = 0;
	my $row = 5;
	while($row < $hchar-13){
		my $conn = $connections[$i++];
		$row += drawSingleConnInfo($conn, $row);
	}
}

sub drawSingleConnInfo {
	my ($conn, $row) = @_;

	cursorPos(1, $row);
	not defined $conn and pc(' ' x ($wchar-2), 'white') and return 1;

	my $len = pc(' Conn. #' . $conn->number(), 'bold white');
	if($conn->isConnected()){
		$len += pc(' ' . $conn->host() . ':' . $conn->port(), 'bold yellow');
	}
	else{
		$len += pc(' ++ closed ++', 'bold red');
		my $reconnRemain = $conn->getReconnectRemaining();
		$reconnRemain and $len += pc(' (reconnect in ' . hrtv($reconnRemain) . ')', 'bold yellow');
	}
	$len += pc(' ' . $conn->currentGroup(), 'bold cyan');
	pc((' ' x ($wchar-$len-2)), 'white');		# blank out to end of line

	cursorPos(1, ++$row);
	$len = pc('       Speed: ', 'bold white');
	$len += pc( sprintf('%-8s', hrsv($conn->currentSpeed()) . 'Bps'), 'bold red');
	$len += pc(' Data: ', 'bold white');
	$len += pc(hrsv($conn->bytesDownloaded()) . 'B', 'bold green');
	$len += pc('  Segments: ', 'bold white');
	$len += pc($conn->segmentsFinished(), 'bold magenta');
	pc((' ' x ($wchar-$len-2)), 'white');		# blank out to end of line
	return 2;
}

#########################################################################################
my $newStatMsg = 0;
sub drawStatusMsgs {
	$showinghelpscreen and return;
	return unless defined($wchar);	# to prevent decoder thread from trying to draw...

	my $row = $hchar - 12;
	my $statuslimit = 10;	# number of lines to show.

	# Pull any decode messages from the queue and append them
	# This might not be the *best* place for this...
	while($conf->usingThreadedDecoding and $decMsgQ->pending > 0){
		statMsg($decMsgQ->dequeue);
	}

	return unless $newStatMsg;
	$newStatMsg = 0;

	# Trim status messages to size
	while( scalar(@statusmsgs) > $statuslimit){
		shift @statusmsgs;
	}
	foreach my $line (@statusmsgs){
		cursorPos(2, $row);
		if(length($line) > ($wchar-4)){
			$line = substr($line, 0, $wchar-4);	# Clip line
		}
		else{
			$line .= (' ' x ($wchar-4-length($line)));
		}
		pc($line, 'white');
		$row++;
	}
	cursorPos(0, $hchar);
	pc("'?' for help> ", 'bold white');
}

#########################################################################################
# Draws a border around the screen.
#########################################################################################
sub drawBorder {
	drawVLine(0);
	drawVLine($wchar);
	drawHLine(0, "top");
	drawHLine(4, "middle");
	drawHLine($hchar - 13, "middle");
	drawHLine($hchar-2, "bottom");
}

sub drawHLine {
	my $ypos = shift;
	my $hpos = shift;
	cursorPos(0, $ypos);
	if ($conf->noansi) {
		pc('+' . ('-' x ($wchar-2)) . '+', 'bold white');
	} 
	else {
		if ($hpos eq "top") {
			pc("\x1B(0" . 'l' . ('q' x ($wchar-2)) . 'k' . "\x1B(B", 'bold white');
		} 
		elsif ($hpos eq "middle") {
			pc("\x1B(0" . 't' . ('q' x ($wchar-2)) . 'u' . "\x1B(B", 'bold white');
		} 
		elsif ($hpos eq "bottom") {
			pc("\x1B(0" . 'm' . ('q' x ($wchar-2)) . 'j' . "\x1B(B", 'bold white');
		}
	}
}
sub drawVLine {
	my $xpos = shift;
	my $height = shift;
	not $height and $height = ($hchar-2);
	foreach(0..$height){
		cursorPos($xpos, $_);
		if ($conf->noansi) {
			pc('|', 'bold white');
		} 
		else {
			pc("\x1B(0" . "x" . "\x1B(B", 'bold white');
		}
	}
}

#########################################################################################
# helper for printing in color (or not)
#########################################################################################
sub pc {
	my ($string, $colstr) = @_;
	not defined($colstr) and $colstr = "white";	# default to plain white
	$conf->daemon and return length($string);
	if($conf->nocolor){
		print $string;
	}
	else{
		print colored ($string, $colstr);
	}
	return length($string);
}

sub clearScreen {
	!$conf->daemon and 
		$terminal->Tputs('cl', 1, *STDOUT);			# clears screen
}

#########################################################################################
# Positions the cursor at x,y.  Looks at $conf->daemon first.
#########################################################################################
sub cursorPos {
	my ($x, $y) = @_;
	not $conf->daemon and 
		$terminal->Tgoto('cm', $x, $y, *STDOUT);
}
#########################################################################################
# Print or log, depending on $conf->daemon
#########################################################################################
sub porl {
	porlp(shift, $conf->daemon);
}

#########################################################################################
# statOrQ - calls statMsg or enqueues the message, based on the value of $dthreadct, 
# which governs if we're using a threaded approach or not.
#########################################################################################
sub statOrQ {
	my $msg = shift;
	if($conf->usingThreadedDecoding){
		$decMsgQ->enqueue($msg);
	}
	else{
		statMsg($msg);
	}
}

#########################################################################################
# Gracefully close down all server connections.
#########################################################################################
sub disconnectAll {
	foreach my $conn (@connections){
		$conn->closeConnection();
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
	$size = 0 unless defined($size);
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
	chomp $pw;
	ReadMode 0; # default
	print "\n";
	return $pw;
}

sub parseAndEnqueueNzbFile {
	my $nzbfilename = shift;
	my $nzbFile = parseNZB($nzbfilename);
	exit unless defined($nzbFile);

	if(not $conf->insane){
		doNZBSanityChecks($nzbFile);
	}
	
	# TODO: FIX THIS!
	#@fsparts = regexAndSkipping(@fsparts);	# It checks options inside too
	regexAndSkipping($nzbFile);
	$queue->queueNzb($nzbFile);
}

#########################################################################################
# Parse NZB file and return a Nzb::NzbFile object
#########################################################################################
sub parseNZB {
	my $nzbfilename = shift;
	my $lognoprint = shift;
	$nzbfiles{'files'}->{basename($nzbfilename)}->{'read'} = 1;	# set flag indicating we've processed it

	my $fullNzbFilename = derivePath($nzbfilename);
	$fullNzbFilename .= '/' unless $fullNzbFilename =~ /\/$/;
 	$fullNzbFilename .= basename($nzbfilename);

	my $parser = new XML::DOM::Parser;
	my @fileset;
	porlp("Loading and parsing nzb file: " . $nzbfilename . "\n", $lognoprint);
	my $nzbdoc;
	eval {
		$nzbdoc = $parser->parsefile($nzbfilename);
	};
	if($@){
		my $errmsg = trimWS($@);
		if($lognoprint){
			statMsg("The nzb file is BROKEN and the XML could not be parsed.");
		}
		else{
			pc("\n");
			pc(" Sorry, but nzb file is broken!  The xml could not be parsed:\n", 'bold yellow');
			pc("\n");
			pc(" $errmsg\n\n", 'bold yellow');
			pc(" *** nzbperl requires valid, well-formed XML documents.\n\n", 'bold red');
		}
		return undef;
	}

	my $files = $nzbdoc->getElementsByTagName("file");
	my $totalsegct = 0;
	foreach my $i (0..$files->getLength()-1){
		my $fileNode = $files->item($i);
		my $subj = $fileNode->getAttributes()->getNamedItem('subject');
		my $postdate = $fileNode->getAttributes()->getNamedItem('date');

		my @groupnames;
		for my $group ($fileNode->getElementsByTagName('group')) {
			push @groupnames, $group->getFirstChild()->getNodeValue();
		}

		my @segments;
		for my $seg ($fileNode->getElementsByTagName('segment')) {
			my $size = $seg->getAttributes()->getNamedItem('bytes')->getValue();
			my $segNumber = $seg->getAttributes()->getNamedItem('number')->getValue();
			push @segments, Nzb::Segment->new($seg->getFirstChild()->getNodeValue(), $size, $segNumber);
		}

		# If segment numbers are present, use them to sort.
		if (defined($segments[0]) && defined($segments[0]->{'number'})){
			@segments = sort {
				$a->number <=> $b->number } @segments;
		}
		
		$totalsegct += scalar @segments;

		push @fileset, Nzb::File->new($subj->getValue(), $postdate->getValue(), \@groupnames, \@segments);
	}
	$nzbdoc->dispose;

	porlp("Loaded $totalsegct total segments for " . $files->getLength() . " file(s).\n", $lognoprint);

	@fileset = sortFilesBySubject($lognoprint, @fileset);	# It checks $sort inside
	#@fileset = resetLastOnNzbFlag(@fileset);

	return Nzb::NzbFile->new($fullNzbFilename, @fileset);
}

#########################################################################################
# Filters out files if there is a filter regex, and skips over files from --skip <n>
#########################################################################################
sub regexAndSkipping {
	my $nzbFile = shift;

	if(defined($conf->filterregex) or defined($conf->ifilterregex)){
		$nzbFile->filterFilesOnSubject($conf->filterregex, $conf->ifilterregex);
	}

	# TODO: MAKE THIS SKIP OPERATE ON THE QUEUE, NOT ON A SINGLE NZB!!!
	if($conf->skipfilect){
		if($conf->skipfilect >= $nzbFile->fileCount()){
			pc("\nWhoops:  --skip " . $conf->skipfilect . " would skip ALL " . $nzbFile->fileCount() . 
					" files...aborting!\n\n", 'bold yellow') and exit 0;
		}
		print "Removing " . $conf->skipfilect . " files from nzb set (--skip " . $conf->skipfilect . ")\n";
		$nzbFile->remove_from_head($conf->skipfilect);
	}
}

#########################################################################################
# Sorts files in a fileset based on the name
#########################################################################################
sub sortFilesBySubject {
	my $quiet = shift;
	my @fileset = @_;
	if(!$conf->nosort){
		porlp("Sorting files by filename (subject)...", $quiet);
		@fileset = 
			sort {
				$a->name cmp $b->name;
			} @fileset;
		porlp("finished.\n", $quiet);
	}
	return @fileset;
}
#########################################################################################
# Derives a path from a filename (passed on commandline).
# The result isn't necessarily absolute, can be relative
#########################################################################################
sub derivePath {
	my $filename = shift;
	if($filename =~ /\//){		# then it has path information, likely not windows compat
		$filename =~ s/(^.*\/).*/$1/;
		return $filename;
	}
	return cwd;
}
#########################################################################################
# Main entry point for NZB file sanity checking
#########################################################################################
sub doNZBSanityChecks {
	my $nzbFile = shift;
	print "Analyzing sanity of NZB file segment completeness...\n";
	my @suspectFiles = getSuspectFiles($nzbFile);
	my $badfilect = scalar @suspectFiles;
	not $badfilect and pc("All files pass segment size sanity checks!  Swell.\n", 'bold green') and return;

	SMENUDONE:
	while(1){
		pc(sprintf("There are %d of %d files that may have missing or broken segments.\n", $badfilect, $nzbFile->fileCount), 'bold yellow');
		pc("It is likely that these files will be unusable if downloaded.\n", 'bold yellow');
		($conf->dropbad or $conf->insane) and return;	# User selection not needed.
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
					$conf->insane(1);
					last SMENUDONE;
				}
				elsif($char =~ /d/){
					print "Setting --dropbad option...\n";
					$conf->dropbad(1);
					last SMENUDONE;
				}
				elsif($char =~ /v/){
					showSuspectDetails(@suspectFiles);
				}
				last;
			}
			else{
				select undef, undef, undef, 0.1;
			}
		}
	}
	if($conf->dropbad){
		dropSuspectFiles($nzbFile, @suspectFiles);
	}
}

#########################################################################################
# Shows details about suspect files...
#########################################################################################
sub showSuspectDetails {
	my @susFiles = @_;
	foreach my $file (@susFiles){
		my $avgsize = $file->avgSegSize();
		print "------------------------------------------------------\n";
		printf(" * File: %s\n", $file->name);
		printf("   Posted on: %s (%d days ago)\n", 
				scalar localtime $file->date,
				(time - $file->date)/(60*60*24) );
		printf("   Adjusted average part size = %d bytes\n", $avgsize);
		my @sids = getSuspectSegments($file);
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
# Looks at the fileset and returns an array of files that are suepect
#########################################################################################
sub getSuspectFiles {
	my $nzbFile = shift;
	my @ret;
	foreach my $file (@{$nzbFile->files()}){
		my $segoffct = 0;
		my @suspectSegInd = getSuspectSegments($file);
		if(scalar @suspectSegInd){
			push @ret, $file;
		}
	}
	return @ret;
}

#########################################################################################
sub getSuspectSegments {
	my $MAX_OFF_PERC = 25;		# Percentage of segment size error/diff to trigger invalid
	my $file = shift;
	my $avg = $file->avgSegSize;
	my @ret;
	my @segs = $file->segments;
	foreach my $i (1..(scalar($file->segments)-1)){  # Last segment is allowed to slide...
		my $seg = $segs[$i-1];
		my $percdiff = 100;
		$avg and $percdiff = 100*(abs($seg->size - $avg)/$avg);
		
		if($percdiff > $MAX_OFF_PERC){
			push @ret, $i-1;
		}
	}
	return @ret;
}

#########################################################################################
sub dropSuspectFiles { 
	my ($nzbFile, @suspectFiles) = @_;
	my @newset; 
	my $dropct = 0; 
	foreach my $file (@suspectFiles){
		printf("Dropping [%s] from filset (suspect)\n", $file->name);
		$nzbFile->drop_file($file);
		$dropct++;
	}
	pc(sprintf("Dropped %d suspect files from NZB (%d files remain)\n", $dropct, $nzbFile->fileCount), 'bold yellow');
	print " -> short delay (for user review)";
	foreach(5,4,3,2,1){
		print "...$_";
		sleep 1;
	}
	print "...let's go!\n";
}


#########################################################################################
# Helper to detect that uudeview is installed.  Always a good idea, ya'know, since we're
# dependant on it!
#########################################################################################
sub haveUUDeview {
	if(defined($conf->uudeview)){	# Given on commandline or config file
		if(-e $conf->uudeview){
			pc("uudeview found: " . $conf->uudeview . "\n", "bold green");
			return 1;
		}
		pc("Warning: uudeview not found at location " . $conf->uudeview . "\n", "bold yellow");
	}
	my @paths = split /:/, $ENV{'PATH'};	# path sep different on winderz?
	foreach my $p (@paths){
		$p =~ s/\/$//;
		$p = $p . "/uudeview";
		if(-e $p){
			pc("uudeview found: $p\n", "bold green");
			$conf->uudeview($p);
			return 1;
		}
	}
	pc("Error: uudeview not found in path...aborting!\n", "bold red");
	return 0;
}

#########################################################################################
# Trim ws on both sides of string.  Undef is ok.
#########################################################################################
sub trimWS {
	my $s = shift;
	return $s unless defined $s;
	$s =~ s/^\s+//;
	$s =~ s/\s+$//;
	return $s;
}

#########################################################################################
# Checks for a newer version, disabled with --noupdate
#########################################################################################
sub checkForNewVersion {
	$conf->noupdate and return;	# they don't want update checking
	print "Checking for availability of newer version...\n";
	eval "use LWP::Simple;";
	if($@){
		print "LWP::Simple is not installed, skipping up-to-date check.\n";
		return;
	}
	my $remote_ver = eval "get \"$UPDATE_URL\"";
	if(!defined($remote_ver)){
		pc("Error fetching current version during update check: $!\n", 'bold red');
		pc("Skipping up-to-date check.\n", 'bold yellow');
		return;
	}

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
		pc("\n");
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
	my ($server, $port) = ($conf->server, $conf->port);
	print <<EOL

  Hi.  This is the nzbperl help screen. 
  You can use the following keys while we're running:

  '1'   : Switch to low bandwidth mode ($lowbw kBps)
  '2'   : Switch to med bandwidth mode ($medbw kBps)
  '3'   : Switch to high bandwidth mode (unlimited)
  '+'   : Nudge target bandwidth setting up 1 kBps
  '-'   : Nudge target bandwidth setting down 1 kBps
  'a'   : Add a new connection
  'd'   : Drop the last connection
  'p'   : Pause/unpause (closes connections)
  's'   : Skip over topmost file in queue
  'c'   : Toggle color on or off
  'q'   : Quit the program (aborts all downloads)
  '?'   : Show this help screen

  Connected to $server:$port
  (Your download is still in progress:  
  
  [ Press any key to return to the main screen ]

EOL
;
	drawVLine(0, 19);
	drawVLine($wchar, 19);
	drawHLine(0, 'top');
	drawHLine(19, 'bottom');

	showHelpScreenEta();
	$showinghelpscreen = 1;
}

sub showHelpScreenEta {
	cursorPos(40, 16);
	pc("ETA: " . getETA(), 'bold green');
	pc(")", 'bold white');
	cursorPos(0, 20);
}
#########################################################################################
# Show program usage
#########################################################################################
sub showUsage {
my $errmsg = shift;
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
 --ssl             : Connect to server using SSL (secure sockets layer).
                   : May be combined with --http_proxy or --socks_server to 
                   : use a proxy server with SSL.
 --socks_server <s>: Connect using <s> as a socks proxy server. Defaults to
                   : port 1080, but can use --socks_server <server:port> to
                   : use an alternative port.
 --http_proxy <s>  : Use <s> as an http proxy server to use.  Defaults
                   : to port 8080, but can use --http_proxy <server:port> to
                   : use an alternative port.
 --proxy_user <u>  : Authenticate to the proxy using <u> as the username
 --proxy_passwd <p>: Use <p> as the proxy user password (otherwise prompted)
 --ipv6            : Use IPv6 sockets for communication
 --keepparts       : Keep all encoded parts files on disk after decoding
 --keepbroken      : Continue downloading files with broken/missing segments
                   : and leave the parts files on disk still encoded.
 --keepbrokenbin   : Decode and keep broken decoded files (binaries) on disk.
 --dlpath <dir>    : Download and decode all files to <dir>  
                   : (default downloads to current dirctory)
 --dlrelative      : Download and decode to the dir that the nzbfiles are in
                   : (default downloads to current directory)
 --dlcreate        : Create download directories per nzb file
 --dlcreategrp     : Create download dirctories with usenet group names
 --queuedir <dir>  : Monitor <dir> for nzb files and queue new ones
 --forever         : Run forever, waiting for new nzbs (requires --queuedir)
 --postdec <prog>  : Run <prog> after each file is decoded, env var params.
 --postnzb <prog>  : Run <prog> after each NZB file is completed.
 --diskfree <perc> : Stop downloading when dir free space above <perc>
 --redo            : Don't skip over existing downloads, do them again
 --insane          : Bypass NZB sanity checks completely
 --dropbad         : Auto-skip files in the NZBs with suspected broken parts
 --skip <n>        : Skip the first <n> files in the nzb (don't process)
 --med <kBps>      : Set "med" bandwidth to kBps (default is 95kBps)
 --low <kBps>      : Set "low" bandwidth to kBps (default is 35kBps)
 --speed <speed>   : Explicitly specify transfer bandwidth in kBps
 --log <file>      : Log status messages into <file> (default = none)
 --decodelog <file>: Append uudeview output into <file> (default = none)
 --dthreadct <ct>  : Use <ct> number of decoder threads.  Set ct = 0 for single
                     threaded perl operation.  (Note: When ct = 0, downloads
                     will be paused during file decoding)
 --daemon          : Run in background as daemon (use log for status)
 --rcport <port>   : Enable remote control functionality on port <port>
 --retrywait <n>   : Wait <n> seconds between reconnect tries (default = 300)
 --nosort          : Don't sort files by name before processing
 --chunksize       : Amount to read on each recv() call (for tweakers only)
                   : Default = 5k, Can specify in bytes or kb (ie. 5120 or 5k)
 --filter <regex>  : Filter NZB contents on <regex> in subject line
 --ifilter <regex> : Inverse filter NZB contents on <regex> in subject line
 --uudeview <app>  : Specify full path to uudeview (default found in \$PATH)
 --nocolor         : Don't use color
 --noansi          : Don't use ANSI characters (text only)
 --noupdate        : Don't check for newer versions at startup
 --help            : Show this screen

  During runtime, press 'h' or '?' to see a list of key commands.

  nzbperl version $version, Copyright (C) 2004 Jason Plumb
  nzbperl comes with ABSOLUTELY NO WARRANTY; This is free software, and 
  you are welcome to redistribute it under certain conditions;  Please 
  see the source for additional details.

EOL
;
if($errmsg and (length($errmsg))){
	print " *****************************************************************\n";
	print " ERROR:\n";
	print " $errmsg\n";
	print " *****************************************************************\n";
}

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   _   _         _                ____                    __   _         
#  | \ | |  ____ | |__    _   _   / ___|   ___    _ __    / _| (_)   __ _ 
#  |  \| | |_  / | '_ \  (_) (_) | |      / _ \  | '_ \  | |_  | |  / _` |
#  | |\  |  / /  | |_) |  _   _  | |___  | (_) | | | | | |  _| | | | (_| |
#  |_| \_| /___| |_.__/  (_) (_)  \____|  \___/  |_| |_| |_|   |_|  \__, |
#                                                                   |___/ 
# Class used to hold configuration data for this app
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
package Nzb::Config;
use strict;
use File::Basename;
use Getopt::Long;
use Cwd;
our $AUTOLOAD;

sub new {
	my %fields = (
		server => '', port => -1, user => '', pw => '', keepparts => 0, 
		keepbroken => 0, keepbrokenbin => 0, help => 0, nosort => 0, overwritefiles => 0, 
		connct => 2, nocolor => 0, logfile => undef, insane => 0, dropbad => 0, skipfilect => 0, 
		reconndur => 300, filterregex => undef, configfile => "$ENV{HOME}/.nzbperlrc", uudeview => undef, daemon => 0, 
		dlrelative => undef, dlpath => undef, noupdate => 0, ssl => undef, socks_server => undef, 
		socks_port => -1, proxy_user => undef, proxy_passwd => undef, http_proxy_server => undef, http_proxy_port => -1, 
		dlcreate => undef, dlcreategrp => undef, noansi => 0, queuedir => undef, rcport => undef,
		postDecProg => undef, postNzbProg => undef, ipv6 => undef, forever => undef, DECODE_DBG_FILE => undef, 
		ifilterregex => undef, dthreadct => 1, diskfree => undef
	);

	my $class = shift;
	$class = ref($class) || $class;
	my $self = {
		_permitted => \%fields,
		%fields
	};
	bless($self, $class);
	return $self;
}

sub DESTROY {}	# empty destructor to prevent autoload proxy from getting it
sub AUTOLOAD {
	my $self = shift;
	my $type = ref($self)
	or die "$self is not an object (called with $AUTOLOAD)";

	my $name = $AUTOLOAD;
	$name =~ s/.*://;   # strip fully-qualified portion

	unless (exists $self->{_permitted}->{$name} ) {
		die "Can't access `$name' field in class $type";
	}

	if (@_) {
		return $self->{$name} = shift;	# Setter
	} 
	else {
		return $self->{$name};			# Getter
	}
}


#########################################################################################
# Parse command line options and assign sane globals etc.
#########################################################################################
sub handleCommandLineOptions {
	my $config = shift;
	# NOTE: We allow args to be passed in to facilitate unit testing
	my @args = @_;	
	my @saveargs = @args;

	my %optionsmap = ('server=s' => \$config->{server}, 'user=s' => \$config->{user}, 'pw=s' => \$config->{pw}, 
					'help' => \$config->{help}, 'med=s' => \$config->{medbw}, 'low=s' => \$config->{lowbw}, 
					'speed=s' => \$config->{targkBps}, 'keepparts' => \$config->{keepparts}, 
					'keepbroken' => \$config->{keepbroken}, 'keepbrokenbin' => \$config->{keepbrokenbin}, 
					'nosort' => \$config->{nosort}, 'redo' => \$config->{overwritefiles}, 'conn=i' => \$config->{connct}, 
					'nocolor' => \$config->{nocolor}, 'log=s' => \$config->{logfile},
					'insane' => \$config->{insane}, 'dropbad' => \$config->{dropbad},
					'skip=i' => \$config->{skipfilect}, 'retrywait=i' => \$config->{reconndur},
					'filter=s' => \$config->{filterregex}, 'config=s' => \$config->{configfile},
					'uudeview=s' => \$config->{uudeview}, 'daemon' => \$config->{daemon}, 'forever' => \$config->{forever},
					'dlrelative' => \$config->{dlrelative}, 'dlpath=s' => \$config->{dlpath}, 'noupdate' => \$config->{noupdate},
					'ssl' => \$config->{ssl}, 'socks_server=s' => \$config->{socks_server}, 'socks_port=i' => \$config->{socks_port},
					'socks_user=s' => \$config->{proxy_user}, 'socks_passwd=s' => \$config->{proxy_passwd},
					'http_proxy=s' => \$config->{http_proxy_server}, 'dlcreate'=>\$config->{dlcreate}, 'dlcreategrp' => \$config->{dlcreategrp},
					'noansi' => \$config->{noansi}, 'queuedir=s' => \$config->{queuedir}, 'rcport=i' => \$config->{rcport},
					'postdec=s' => \$config->{postDecProg}, 'postnzb=s' => \$config->{postNzbProg},
					'ipv6' => \$config->{ipv6}, 'chunksize=s' => \$config->{recv_chunksize}, 'decodelog=s' => \$config->{DECODE_DBG_FILE},
					'ifilter=s' => \$config->{ifilterregex}, 'dthreadct=s' => \$config->{dthreadct}, 'diskfree=s' => \$config->{diskfree});

	# This extra call is required to set up the --config option, expected below
	@ARGV=@args;
	GetOptions(%optionsmap);
	
	my $errmsg;
	# This is the facility for trapping stderr from GetOptions, so that we 
	# can pretty print it at the bottom of the help screen.
	local $SIG{'__WARN__'} = sub {
		$errmsg = $_[0];
		chomp $errmsg;
	};

	# First see if the config file is there, if so, slurp options from it.
	my $optionsAreOk;
	if(-e $config->configfile){
		@args = $config->readConfigFileOptions($config->configfile);
		@ARGV = @args;
		$optionsAreOk = eval 'GetOptions(%optionsmap)';
		return $errmsg unless $optionsAreOk;
	}
	else {
		print "Config file " . $config->configfile . " does not exist.  Skipping.\n";
	}
	
	# Now restore the commandline args and parse those (overriding config file options)
	@args = @saveargs;	# restore
	@ARGV = @args;
	$optionsAreOk = eval 'GetOptions(%optionsmap)';
	return $errmsg unless $optionsAreOk;
	if($config->help){
		return "";
	}
	$config->nocolor and $config->usecolor(0);

	not $optionsAreOk and return "";

	if($config->usingThreadedDecoding){
		eval "
		use threads;
		use Thread::Queue;";
		($@) and return "ERROR: Could not use Perl thread modules.\r\n" .
		" Try setting --dthreadct 0 to run with a single threaded Perl.";
	}

	if($recv_chunksize =~ /kb?$/i){
		$recv_chunksize =~ s/kb?$//i;
		$recv_chunksize = $recv_chunksize*1024;
	}

	if(defined($config->queuedir) and (not $config->queuedir =~ /^\//)){
		return "--queuedir must specify an ABSOLUTE (not relative) path.";
	}
	if(not $args[0] and (not defined $config->queuedir)){		# No NZB file given?
		return "Missing nzb file or directory queue.";
	}

	if($config->server =~ /:\d+$/){
		($config->{server}, $config->{port}) = hostport($config->server);
	}
	if(not length($config->server)){
		$config->server($ENV{'NNTPSERVER'});
		not $config->server and return "Must provide --server or set \$NNTPSERVER environment";
	}
	$config->{server} = Nzb::trimWS($config->server);

	$config->{dlpath} = cwd unless (defined($config->dlpath) or defined($config->dlrelative));
	if($config->dlpath and not $config->dlpath =~ /^\//){
		return "--dlpath must specify an ABSOLUTE (not relative) path.";
	}

	# Make sure that dlpath ends with a slash
	if($config->dlpath and (not ($config->dlpath =~ /\/$/))){
		$config->{dlpath} .= '/';
		($config->dlpath =~ m#^([\w\d\s\.\_\-\/\\]+)$#) and $config->{dlpath} = $1;	# untaint dlpath
	}

	if($config->dropbad and $config->insane){	# conflicting
		return "Error: --dropbad and --insane are conflicting (choose one)";
	}

	if($config->forever and not (defined($config->rcport) or defined($config->queuedir))){
		return "Error: --forever requires either --queuedir or --rcport.\n" .
				" Please choose one and try again.";
	}

	if(defined($config->queuedir) and !$config->dropbad and !$config->insane){
		return "Use of --queuedir requires either --dropbad or --insane.\n" . 
				" Please choose one and try again.";
	}

	if(defined($config->postDecProg) and not -e $config->postDecProg){
		return "--postdec program \"" . $config->postDecProg . "\" does not exist.\n" . 
				" Please confirm the program and try again.";
	}

	if($config->dlpath and $config->dlrelative){ # conflicting options
		return "Error: --dlrelative and --dlpath <dir> are conflicting (choose one)";
	}

	# Verify that output dir is writable...
	if(defined($config->dlpath) and not -w $config->dlpath) {
		return "Error: dlpath '" . $config->dlpath . "' is not writable!\n" .
				" Please change the permissions or use a different directory.";
	}

	if(defined($config->DECODE_DBG_FILE)){
		if(open(DBGTMP,">" . $config->DECODE_DBG_FILE)){
			close DBGTMP;	#all good
		}
		else{
			return "The decode log file '" . $config->DECODE_DBG_FILE. "' is unwritable!";
		}
	}

	if($config->port == -1) {
	    if (defined($config->ssl)) {
			(undef, undef, $config->{port}, undef) = getservbyname("nntps", "tcp");
	    } 
		else {
			(undef, undef, $config->{port}, undef) = getservbyname("nntp", "tcp");
	    }
	}

	if(defined($config->socks_server) and defined($config->http_proxy_server)){
		return "Error: --socks_server and --http_proxy are conflicting (choose one)";
	}
	if(defined($config->dlcreate) and defined($config->dlcreategrp)){
		return "Error: --dlcreate and --dlcreategrp are conflicting (choose one)";
	}

	if (defined($config->ssl)) {
	    eval "use IO::Socket::SSL;";		# use module only if option is enabled.
		($@) and return "ERROR: --ssl was specified, but IO::Socket::SSL isn't available.\r\n" . 
						" Please install IO::Socket::SSL to use --ssl and try again.";
	}

	if (defined($config->socks_server)) {	
	    eval "use IO::Socket::Socks;"; 		# use module only if option enabled 
		($@) and return "ERROR: --socks_server was specified, but IO::Socket::Socks isn't available.\r\n" . 
						" Please install IO::Socket::Socks to use a SOCKS server and try again.";

	    if ($config->socks_port == -1) {
			if($config->socks_server =~ /:\d+$/){
				($config->{socks_server}, $config->{socks_port}) = hostport($config->socks_server);
			} 
			else {
				(undef, undef, $config->{socks_port}, undef) = getservbyname("socks", "tcp");
			}
	    }
		$config->{socks_server} = Nzb::trimWS($config->socks_server);
	}

	if (defined($config->http_proxy_server)) {
	    eval "use Net::HTTPTunnel;";		# use module only if option enabled
		($@) and return "ERROR: --http_proxy was specified, but Net::HTTPTunnel isn't available.\r\n" . 
						" Please install Net::HTTPTunnel to use an HTTP proxy and try again.";

		if($config->http_proxy_server =~ /:\d+$/){
			($config->{http_proxy_server}, $config->{http_proxy_port}) = hostport($config->http_proxy_server);
		} 
		else {
			(undef, undef, $config->{http_proxy_port}, undef) = getservbyname("webcache", "tcp");
		}
		$config->{http_proxy_server} = Nzb::trimWS($config->http_proxy_server);
	}

	if(defined($config->ipv6)){
	    eval "use IO::Socket::INET6;";		# use ipv6 module if option given
		($@) and return "ERROR: --ipv6 was given and the IO::Socket::INET6 module could not be found.\r\n" . 
						" You must install the IO::Socket::INET6 module to use IPv6";
	}

	return undef;	# success
}

sub hostport {
	my $class = shift;
	my ($host, $port) = ($_[0], -1);
	if($host =~ /:\d+$/){
		$port = $host;
		$port =~ s/.*://;
		$host =~ s/:.*//;
	}
	return ($host, $port);
}


#########################################################################################
# Reads options from the config file and tucks them into an array which is returned.
# Used by caller to make it seems as though the options were all passed on the command
# line.
#########################################################################################
sub readConfigFileOptions(){
	my $config = shift;
	my $filename = shift;
	$config->configfile($filename);
	print "Reading config options from $filename...\n";
	open CFG, "<$filename" or die "Error opening $filename for config options";
	my $line;
	my @opts;
	while($line = <CFG>){
		chomp $line;
		$line =~ s/^\s+//;
		$line =~ s/^-+//;				# In case dashes in config file
		$line =~ s/(\s+)?=(\s+)?/=/;	# Remove whitespace around equals sign
		next if $line =~ /^#/;
		next unless length($line);
		push @opts, "--$line";
	}
	close CFG;
	return @opts;
}

#########################################################################################
# Simple helper to determine if we're using threaded or nonthreaded decoding.
# It looks at the dthreadct variable and returns 1 if dthreadct > 0.
#########################################################################################
sub usingThreadedDecoding {
	my $conf = shift;
	return ($conf->dthreadct > 0);
}

#########################################################################################
# Figures out where a file will be going on disk.  Returns the directory.
#########################################################################################
sub getDestDirForFile {
	my ($conf, $file) = @_;
	my $ret;
	if(defined($conf->dlpath)){
		$ret = $conf->dlpath;
		if (defined($conf->dlcreate)) {	# if we like to create nicely organized subdirs
			my $nzbbase = basename($file->parentNzb()->nzbFile());
			if ($nzbbase =~ /msgid_[0-9]*_(.*).nzb/) {  # Filter name from NewzBin style names
				$nzbbase = $1;
			} 
			elsif ($nzbbase =~ /(.*).nzb$/i) { # Strip the .nzb extension and 
				$nzbbase = $1;
			} 
			$ret = $conf->dlpath . '/' . $nzbbase;
		} 
		elsif (defined($conf->dlcreategrp)){
			$ret = $conf->dlpath . '/' . $file->groups()->[0];
		}
	}
	elsif(defined($conf->dlrelative)){
		$ret = dirname($file->parentNzb()->nzbFile);
	}
	$ret =~ s/\/$//;
	return $ret;
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  _   _         _                ___                                
# | \ | |  ____ | |__    _   _   / _ \   _   _    ___   _   _    ___ 
# |  \| | |_  / | '_ \  (_) (_) | | | | | | | |  / _ \ | | | |  / _ \
# | |\  |  / /  | |_) |  _   _  | |_| | | |_| | |  __/ | |_| | |  __/
# |_| \_| /___| |_.__/  (_) (_)  \__\_\  \__,_|  \___|  \__,_|  \___|
#                                                              
# Represents the queue of nzb files of files of parts
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
package Nzb::Queue;
use strict;
use fields;
use Data::Dumper;

sub new {
	my $class = shift;
	my @files;
	my @newFilenames;			# unparsed new filenames
	my @seenFilenames;			# filenames already parsed/handled
	my $self = {
		files => \@files,		# our queue of active Nzb::NzbFile objects
		nzbFileIndex => 0,
		fileIndex => 0,
		segmentIndex => -1,
		newFilenames => \@newFilenames,
		seenFilenames => \@seenFilenames
	};
	bless($self, $class);
	Nzb::Printer->import(qw(statMsg));
	return $self;
}

# Give me an nzb file (Nzb::NzbFile) and I'll add it to the queue
# Returns a count of the files in the queue
sub queueNzb {
	my ($queue, $nzbfile) = @_;
	return scalar @{$queue->{files}} unless $nzbfile;	# Don't add undef param file

	#printf("DEBUG: haveSeen for %s gives %s\n", $nzbfile->nzbFile, $queue->haveSeen($nzbfile->nzbFile));
	$queue->haveSeen($nzbfile->nzbFile) and return scalar @{$queue->{files}};
	push @{$queue->{seenFilenames}}, $nzbfile->nzbFile;

	push @{$queue->{files}}, $nzbfile;
	return scalar @{$queue->{files}};
}

sub haveSeen {
	my ($queue, $filename) = @_;
	return scalar grep(/$filename/, @{$queue->{seenFilenames}});
}

# Adds a new filename to the list of unparsed nzb files, but only
# if it hasn't been seen before.
# Returns the number of new filenames.
sub addNewFilename {
	my ($queue, $filename) = @_;
	if(not $queue->haveSeen($filename)){
		push @{$queue->{newFilenames}}, $filename;
	}
	return $queue->pendingNzbFileCount;
}

sub nzbFiles {		# eek, leaky abs
	my $queue = shift;
	return $queue->{files};
}

sub isEmpty {
	my $queue = shift;
	return (scalar @{$queue->{files}} == 0) and 
			($queue->pendingNzbFileCount() == 0);
}

# Return a count of how many unparsed nzb files are remaining in the queue (still on disk).  
sub pendingNzbFileCount {
	my $queue = shift;
	return scalar @{$queue->{newFilenames}};
}

# Return a count of pending files (Nzb::File) within all nzbs (Nzb::NzbFile)
sub pendingFileCount {
	my $queue = shift;
	my $ret = 0;
	foreach my $nzb (@{$queue->{files}}){
		$ret += $nzb->fileCount();	
	}
	return $ret;
}

sub haveAlreadyQueued {
	my ($queue, $filename) = @_;
	die "TODO: BUILD THIS.";
}

#########################################################################################
# Determines the total file size for all segments in the NZB file
#########################################################################################
sub computeTotalNzbSize {
	my $queue = shift;
	my $tot = 0;
	foreach my $file (@{$queue->{files}}){
		$tot += $file->totalSize();
	}
	return $tot;
}

#########################################################################################
# Looks at the queue and finds the next segment to download.  If there
# are no more segments to download, the queue is considered empty and 
# this will return undef.
#########################################################################################
sub getNextSegment {
	my $queue = shift;
	$queue->isEmpty() and return undef;
	my $newfile = 0;

	while(1){
		$queue->{segmentIndex} < 0 and ($newfile = 1);
		$queue->{segmentIndex}++;

		if($queue->{nzbFileIndex} >= scalar @{$queue->{files}}){	# Roll to next (pending?) nzb file

			# TODO: Also need to see if we have files waiting to be parsed into us first
			return undef;
		}

		my $nzb = $queue->{files}->[$queue->{nzbFileIndex}];
		if($queue->{fileIndex} >= $nzb->fileCount()){
			$queue->{segmentIndex} = -1;
			$queue->{fileIndex} = 0;
			$queue->{nzbFileIndex}++;
			next;
		}

		my $file = $nzb->{fileSet}->[$queue->{fileIndex}];

		if(Nzb::hitDiskSpaceLimit($file)){	# Do free space checking if option set
			return undef;
		}

		if($queue->{segmentIndex} >= $file->segmentCount()){	# Rolling to next file
			$queue->{segmentIndex} = -1;
			$queue->{fileIndex}++;
			next;
		}

		# Skip over segments if we already have this file or if it's broken or
		# if the user flagged it to skip.
		# Paying attention to options as needed
		if( $file->skipped() or
			(!$conf->overwritefiles and $file->existsOnDisk()) or
			(!$conf->keepbroken and !$conf->keepbrokenbin and $file->isBroken())){
			$file->{segments}->[$queue->{segmentIndex}]->markAsComplete();
			next;
		}

		$newfile and statMsg(sprintf("Starting next file: %s", $file->name));
		return $file->{segments}->[$queue->{segmentIndex}];
	}
}

#######################################################################################
# Removes all completed files from the queue (via their nzbs) and returns them in an 
# array of Nzb::File
#######################################################################################
sub popCompletedFiles {
	my $queue = shift;
	my @ret = ();
	foreach my $nzb (@{$queue->{files}}){
		my $fileInd = 0;
		foreach my $file (@{$nzb->{fileSet}}){
			if($file->allSegmentsAreFinished()){
				push @ret, $file;

				# Need to bump down our file index if we're removing a file that's before our index!
				$queue->{fileIndex}-- unless ($fileInd > $queue->{fileIndex});

				$nzb->removeFile($file);
			}
			$fileInd++;
		}
		if($nzb->fileCount == 0){		# This nzb file is done
			# TODO: Also need to see if we have files waiting to be parsed into us first
			shift @{$queue->{files}};
		}
	}
	return @ret;
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  _   _         _               _   _         _       _____   _   _        
# | \ | |  ____ | |__    _   _  | \ | |  ____ | |__   |  ___| (_) | |   ___ 
# |  \| | |_  / | '_ \  (_) (_) |  \| | |_  / | '_ \  | |_    | | | |  / _ \
# | |\  |  / /  | |_) |  _   _  | |\  |  / /  | |_) | |  _|   | | | | |  __/
# |_| \_| /___| |_.__/  (_) (_) |_| \_| /___| |_.__/  |_|     |_| |_|  \___|
#
# Represents an nzb file
# TODO: Consider moving loading/parsing/sorting/sanity checking etc. into here
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
package Nzb::NzbFile;
use strict;

sub new {
	my ($class, $filename, @fileset) = @_;
	my $nzb = {
		nzbFile => $filename,
		fileSet => \@fileset
	};
	bless($nzb, $class);
	foreach my $file (@{$nzb->{fileSet}}){
		$file->parentNzb($nzb);
	}
	return $nzb;
}

sub fileCount {
	my $nzb = shift;
	return scalar @{$nzb->{fileSet}};
}

sub totalSize {
	my $nzb = shift;
	my $tot = 0;
	foreach my $file (@{$nzb->{fileSet}}){
		$tot += $file->totalSize;
	}
	return $tot;
}

sub nzbFile {
	return $_[0]->{nzbFile};
}

sub files {	# erg, leaky abs
	my $nzb = shift;
	return $nzb->{fileSet};
}

sub removeFile {
	my ($nzbFile, $file) = @_;
	my $i = 0;
	foreach my $testFile (@{$nzbFile->{fileSet}}){
		if($testFile == $file){
			splice @{$nzbFile->{fileSet}}, $i, 1;
			return;
		}
		$i++;
	}
}

#########################################################################################
# Filters the files in this nzb based on the subject.  Returns the number of files
# that were REMOVED from the set.
#########################################################################################
sub filterFilesOnSubject {
	my ($nzbFile, $inclusiveRegex, $exclusiveRegex) = @_;
	print "Filtering files with regular expressions...\n";
	my $orgsize = scalar @{$nzbFile->{fileSet}};
	my @nset;
	while($nzbFile->fileCount() > 0){
		my $f = shift @{$nzbFile->{fileSet}};
		if( (defined($inclusiveRegex) and ($f->name =~ /$inclusiveRegex/)) or
			(defined($exclusiveRegex) and (not $f->name =~ /$exclusiveRegex/))){
			push @nset, $f;
		}
	}
	if(scalar @nset < 1){
		print "\nWhoops:  Filter removed all files (nothing left)...aborting!\n\n";
		exit 0;
	}
	printf("Kept %d of %d files (filtered %d)\n", scalar(@nset), $orgsize, $orgsize - scalar(@nset));

	$nzbFile->{fileSet} = \@nset;
	return $orgsize - scalar(@nset);
}

# Removes the first <n> files from this nzb.
# TODO: Get rid of this if/when skipping is moved into the queue (not per nzb)
sub remove_from_head {
	my ($nzbFile, $count) = @_;
	while($count > 0){
		shift @{$nzbFile->{fileSet}};
		$count--;
	}
}

sub drop_file {
	my ($nzbFile, $targetFile) = @_;
	my $i = 0;
	foreach my $file ( @{$nzbFile->{fileSet}}){
		if($file == $targetFile){
			splice @{$nzbFile->{fileSet}}, $i, 1;
			return;
		}
		$i++;
	}
	die "Couldn't locate the file to drop from the nzb set...";
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  _   _         _                ____                                         _     _         
# | \ | |  ____ | |__    _   _   / ___|   ___    _ __    _ __     ___    ___  | |_  (_)   ___    _ __  
# |  \| | |_  / | '_ \  (_) (_) | |      / _ \  | '_ \  | '_ \   / _ \  / __| | __| | |  / _ \  | '_ \ 
# | |\  |  / /  | |_) |  _   _  | |___  | (_) | | | | | | | | | |  __/ | (__  | |_  | | | (_) | | | | |
# |_| \_| /___| |_.__/  (_) (_)  \____|  \___/  |_| |_| |_| |_|  \___|  \___|  \__| |_|  \___/  |_| |_|
#  
# Represents a connection to a server
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
package Nzb::Connection;
use strict;
use fields;
use File::Basename;
use Socket;
use Data::Dumper;	# TODO: Remove me when done
use IO::Socket::INET;

sub new {
	my ($class, $number, $host, $port, $user, $pass) = @_;
	my $self = {
		number => $number,	# this connection's number
		host => $host, port => $port,
		user => $user, pass => $pass,
		sock => undef,		# the socket for comms
		currentGroup => 'x', # the current group we're in (default to a non group)
		segment => undef,	# the segment this connection is working on
		buff => undef,		# the buffer for body data
		tmpfile => undef,	# temp file we're writing to
		bytesDownloaded => 0,
		segmentsFinished => 0,
		lastGetResponse => undef,	# last server response line	
		lastConnectRetryTime => undef, #
		lastDataTime => undef		# used in bw calcs
	};
	@{$self->{dataReadSizes}} = ();		# used in bw calcs
	@{$self->{dataReadTimes}} = ();		# used in bw calcs
	bless($self, $class);

	Nzb::Printer->import( qw(porlp statMsg));
	return $self;
}

sub number {
	return $_[0]->{number};
}

sub bytesDownloaded {
	return $_[0]->{bytesDownloaded};
}

sub segmentsFinished {
	return $_[0]->{segmentsFinished};
}

sub currentGroup {
	return defined($_[0]->{currentGroup}) ? $_[0]->{currentGroup} : '';
}
sub host {
	return $_[0]->{host};
}
sub port {
	return $_[0]->{port};
}

#########################################################################################
# Connect to the server and return the connection response string
#########################################################################################
sub connect {
	my ($conn, $silent) = @_;
	porlp(sprintf('Attempting connection #%d to %s:%d...', $conn->{number}, $conn->{host}, $conn->{port}), $silent);
	my $osock = $conn->_createNNTPClientSocket();
	if(!$osock){
		porlp("Connection FAILED!\n", $silent);
		return (undef, "Error connecting to server: '$!'");
	}
	porlp("success!\n", $silent);
	$conn->{sock} = $osock;
	@{$conn->{lastDataTime}} = Time::HiRes::gettimeofday();		# start off with this marker for bw time
	return $conn->blockReadLine();	# read server connection/response string
}

sub isConnected {
	my $conn = shift;
	return defined($conn->{sock});
}

#########################################################################################
# Encapsulates creating a socket for use with NNTP.
#########################################################################################
sub _createNNTPClientSocket {
	my $conn = shift;
	my $paddr = sprintf("%s:%d", $conn->{host}, $conn->{port});
	my %opts = (PeerAddr => $paddr, Proto => 'tcp', Type => SOCK_STREAM);
	#$ipv6 and return IO::Socket::INET6->new(%opts); #TODO: Build me in subclass
	return IO::Socket::INET->new(%opts);
}

#########################################################################################
# Adds our socket, if we have one, to the given select object.  Used to help cover our
# socket without providing direct access to it.
#########################################################################################
sub addSocketToSelect {
	my ($conn, $select) = @_;
	return 0 unless defined $conn->{sock};
	$select->add($conn->{sock});
	return 1;
}
#########################################################################################
# Given a list of "ready" filehandles from a select, determine if we're one of them and
# if we are, try and read a chunk of data into our buffer.  Return the number of chars
# read, 0 on error.
#########################################################################################
sub readDataIfAvailable {
	my ($conn, @ready, @errorset) = @_;

	$conn->_maybeReconnect();

	return unless $conn->_canReadWithoutBlocking(@ready, @errorset);

	my $buff;
	my $recvret = recv($conn->{sock}, $buff, $recv_chunksize, 0);
	if(defined($recvret) and (length $buff > 0)){
		$recvret = length $buff;
	}
	else{	# error reading data!
		$recvret = 0;
		$conn->_handleRemoteDisconnect();
	}

	$totals{'total bytes'} += length($buff);
	$conn->currentFile()->addDownloadedBytes(length($buff));

	$conn->_updateBandwidthInfo(length($buff));
	$conn->{buff} .= $buff;
	$conn->_handleNewIncomingData();
	return $recvret;
}

sub _handleRemoteDisconnect {
	my $conn = shift;
	$conn->_closeSocket();
	$conn->_setLastConnectRetryTime();
	$conn->{buff} = undef;
	statMsg(sprintf('* Remote disconnect on connection #%d', $conn->number));
}

sub _updateBandwidthInfo {
	my ($conn, $length) = @_;
	$conn->{bytesDownloaded} += $length;
	push @{$conn->{dataReadSizes}}, $length;
	push @{$conn->{dataReadTimes}}, Time::HiRes::tv_interval($conn->{lastDataTime});
	if(scalar @{$conn->{dataReadSizes}} > 100){
		shift @{$conn->{dataReadSizes}};
		shift @{$conn->{dataReadTimes}};
	}
	@{$conn->{lastDataTime}} = Time::HiRes::gettimeofday();
}
#########################################################################################
# _handleNewIncomingData - Called when we read new data from the server in response
# to a BODY request. Assumes new data has been added to the connection buffer.
#########################################################################################
sub _handleNewIncomingData {
	my $conn = shift;

	return unless defined($conn->{buff}) and 
		length($conn->{buff}) and defined($conn->{tmpfile});

	while(1){
		my $line = $conn->_popLineFromBuffer();
		last unless defined($line);

		if(not defined($conn->{lastGetResponse})){	# don't have a response to your BODY yet
			$conn->_handleBodyResponseLine($line);
			next;
		}

		my $file = $conn->currentFile();

		# Try and detect the "real" filename
		if(not defined($file->trueFilename())){
			my $tfn = $conn->_getTrueFilename($line);
			if($tfn){
				$file->trueFilename($tfn);
				statMsg(sprintf('Conn. #%d: Found true filename: %s', $conn->number, $tfn));

				#die "DEBUG: Not ready for this yet...";
				#my $targdir = $conn->{'file'}->getDestDirForFile(); # where the file is going on disk
				#makeTargetDirIfNecessary($targdir);
				
				my $tfndisk = sprintf("%s/%s", $conf->getDestDirForFile($file), $tfn);

				if(-e $tfndisk){
					$file->existsOnDisk(1);		# set flag indicating we already have this file

					# TODO/DEBUG: have to update stats when we flag an existing guy

					if(!$conf->overwritefiles){
						statMsg(sprintf('Conn. #%d: File already exists on disk (skipping after segments complete)', $conn->number));
					}
				}
			}
		}
		
		if($line =~ /^\.\r\n/){		# detect end of BODY..
			
			#	$totals{'total file ct'}--;
			#	$totals{'total bytes'} -= $conn->{'filebytes'}; # Remove bytes downloaded
			#	$totals{'total size'} -= $conn->{'file'}->{'totalsize'}; # Remove file bytes from job total 

			$conn->{segment}->markAsComplete();
			$conn->{segmentsFinished}++;
			 
			undef $conn->{tmpfile};				# causes a close
			undef $conn->{segment};				# finish segment

			if($paused){
				statMsg(sprintf('Conn. #%d: Closing down due to pause.', $conn->number()));
				$conn->_protoDisconnect();
			}

			last;
		}
		else{
			$line =~ s/^\.\././o;
			print {$conn->{tmpfile}} $line;
		}
	}
}

sub currentSpeed {
	my $conn = shift;
	my ($sumbytes, $sumtimes) = (0, 0);
	for(my $i=0; $i < scalar@{$conn->{dataReadSizes}}; $i++){
		$sumbytes += $conn->{dataReadSizes}->[$i];
		$sumtimes += $conn->{dataReadTimes}->[$i];
	}
	return $sumbytes ? $sumbytes/$sumtimes : 0;
}
#########################################################################################
# Heuristically determines the "true" filename.  Returns filename or undef
#########################################################################################
sub _getTrueFilename {
	my ($conn, $line) = @_;
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
# Called when we're first getting a response to a BODY request.
#########################################################################################
sub _handleBodyResponseLine {
	my ($conn, $line) = @_;
	#statMsg("DEBUG: Started getting response: $line");
	$conn->{lastGetResponse} = $line;

	my ($mcode, $msize, $mbody, $mid) = split /\s+/, $line;

	# We're just starting, need to slurp up 222 (or other) response
	if($line =~ /^2\d\d\s.*\r\n/s){
		# Bad case where server sends a 5xx message after a 2xx (222)
		if(!$msize and ($conn->{buff} =~ /^5\d\d /)){
			# Handle this error condition (display message to user)
			my $errline = $conn->{buff};
			$errline =~ s/\r\n.*//s;
			statMsg(sprintf('Conn. #%d: Server returned error: %s', $conn->number, $errline));
		}
		else{
			$conn->{'segbytes'} = length($conn->{'buff'});
		}
	}
	else{ # This is an error condition -- often when the server can't find a segment
		$line =~ s/\r\n$//;
		statMsg( sprintf('Conn. #%d FAILED to fetch part #%d (%s)', $conn->number, 
						$conn->{segment}->number, $line));

		$conn->currentFile()->isBroken(1);
		$conn->{segment}->markAsComplete();

		#$conn->{'bstatus'} = 'finished';  # Flag BODY segment as finished

		# Ok, so now that a segment fetch FAILED, we need to determine how to continue...
		# We will look at the keep variables to determine how to continue...
		# If keepbroken or keepbrokenbin are set, we will keep downloading parts...otherwise we will bump
		# up the segnum so that we skip all remaining segments (if any)
		
		if($conf->keepbroken or $conf->keepbrokenbin){		# If we shound continue downloading this broken file
			# Subtract the size of the current segment from the totals
			# (for this file and for the grand totals)
			#my $failedsegsize = @{$conn->{'file'}->{'segments'}}[$conn->{'segnum'}]->{'size'};
			#$totals{'total size'} -= $failedsegsize ;
			#$conn->{'file'}->{'totalsize'} -= $failedsegsize;
			
			# DEBUG TODO: die "This needs to be rebuilt!"; -- update stats!
		}
		else{
			statMsg(sprintf('Conn. #%d aborting file (failed to fetch segment #%d)', 
					$conn->number, $conn->{segment}->number));
			
			# Adjust totals due to skipping failed file
			#$totals{'total file ct'}--;
			#$totals{'total bytes'} -= $conn->{'filebytes'}; # Remove bytes downloaded
			#$totals{'total size'} -= $conn->{'file'}->{'totalsize'}; # Remove file bytes from job total 


			#$conn->{'segnum'} = scalar @{$conn->{'file'}->{'segments'}} - 1;
			#undef $conn->{'tmpfile'};	# causes a close
			#unlink $conn->{'tmpfilename'};
			$conn->{segment} = undef;
		}
	}

}
#########################################################################################
# "pop"s the first line off the connection's buffer, or returns undef if we don't have
# a complete line in the buffer.
#########################################################################################
sub _popLineFromBuffer {
	my $conn = shift;
	return undef unless defined $conn->{buff};
	my $ind1 = index $conn->{buff}, "\r\n";
	return undef unless $ind1 >= 0;
	return substr $conn->{buff}, 0, $ind1+2, '';
}
#########################################################################################
# Determines if we can read from our socket without blocking.  Checks to see if we're
# connected and if our socket file handle is in the "ready" list given us.
#########################################################################################
sub _canReadWithoutBlocking {
	my ($conn, @ready, @errorset) = @_;
	return 0 unless $conn->isConnected();
	if($conn->_connInSelectSet(@errorset)){
		$conn->_handleRemoteDisconnect();
		return 0;
	}
	return $conn->_connInSelectSet(@ready);
}

#########################################################################################
# Determines if this connections socket is a member of the given "selector" set.
#########################################################################################
sub _connInSelectSet {
	my ($conn, @set) = @_;
	foreach my $fh (@set){
		($fh == $conn->{sock}) and return 1;
	}
	return 0;
}
#########################################################################################
# Reads a line from the socket in a blocking manner.
#########################################################################################
sub blockReadLine {
	my $conn = shift;
	my $sock = $conn->{sock};
	my ($line, $buff) = ('', '');
	while(1){
		defined(recv($sock, $buff, 1024, 0)) or last;
	    $line .= $buff;
	    last if $line =~ /\r\n$/;
	}
	return $line;
}

#########################################################################################
# Socket send that can handle both SSL and regular socket...
#########################################################################################
sub sockSend {
	my ($conn, $msg) = @_;
	send $conn->{sock}, $msg, 0;
	#if (ref($sock) eq "IO::Socket::SSL") {	# TODO: Make me workw ith SSL in subclass
	#   $sock->syswrite($msg, undef);
	#} 
	#else {
	#	send $sock, $msg, 0;
	#}
}

#########################################################################################
# Attempts to do a login
#########################################################################################
sub performLogin {
	my ($conn, $silent) = @_;
	return unless $conn->{sock};
	not $silent and printf("Attempting login on connection #%d...", $conn->{number});

	$conn->sockSend("AUTHINFO USER " . $conn->{user} . "\r\n");

	my $line = $conn->blockReadLine();
	if($line =~ /^381/){
		
		$conn->sockSend("AUTHINFO PASS " . $conf->pw . "\r\n");
		
		$line = $conn->blockReadLine();
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
	return 1;
}

sub closeConnection {
	my $conn = shift;
	printf("Closing down connection #%d...", $conn->number);
	not $conn->isConnected() and print "(already closed)\n" and return;

	my $line = $conn->_protoDisconnect();
	$line =~ /^205/ and print "closed gracefully!";
	print "\n";

}

sub _protoDisconnect {
	my $conn = shift;

	$conn->sockSend("QUIT\r\n");

	my $line = $conn->blockReadLine();

	$conn->_closeSocket();

	# TODO: Build this into the ssl subclass
	#if (ref($sock) eq "IO::Socket::SSL") {
	#	$sock->shutdown( 2 );
	#	$sock->close( SSL_no_shutdown => 1);
	#} 
	
	return $line;
}

sub _closeSocket {
	my $conn = shift;
	close($conn->{sock});
	$conn->{sock} = undef;
	$conn->{currentGroup} = undef;
}

sub hasSegment {
	my $conn = shift;
	return defined($conn->{segment});
}

# Returns the parent file for this connection's segment, or undef if not available.
sub currentFile {
	my $conn = shift;
	return $conn->hasSegment() ? $conn->{segment}->parentFile() : undef;
}

sub startNewSegment {
	my ($conn, $segment) = @_;
	$conn->{segment} = $segment;
	$conn->{buff} = undef;
	$conn->{lastGetResponse} = undef;
	my $tmpfile = $conn->_buildTempFilename($segment);

	Nzb::makeTargetDirIfNecessary(dirname($tmpfile));
	if(not -w dirname($tmpfile)){
		statMsg(sprintf("*** ERROR: nzb path %s is not writable!  There will be failures!", dirname($tmpfile)));
		statMsg("*** Please change the permissions or use --dlpath instead of --dlrelative.");
	}

	open $conn->{tmpfile}, ">$tmpfile" or 
		(statMsg("*** ERROR opening $tmpfile (critical!)") and return);
	statMsg("Opened temp file $tmpfile");
	binmode $conn->{tmpfile};

	$conn->_maybeReconnect();
	$conn->_maybeChangeGroup($segment->parentFile());

	# Now request the body for this segment
	$conn->sockSend('BODY <' . $segment->id . ">\r\n");
}

sub _maybeReconnect {
	my $conn = shift;
	$conn->isConnected() and return;	# no work to do

	return unless $conn->getReconnectRemaining() <= 0;		# Bail out if too soon to reconnect

	$conn->_setLastConnectRetryTime();

	statMsg(sprintf('Connection #%d attempting reconnect to %s:%d...', $conn->number, $conn->{host}, $conn->{port}));
	my $osock = $conn->_createNNTPClientSocket();
	if(!$osock){
		statMsg(sprintf('Connection #%d reconnect failed: %s', $conn->number(), $!));
		return;
	}
	statMsg(sprintf('Connection #%d reconnected successfully.', $conn->number));
	$conn->{lastConnectRetryTime} = undef;
	$conn->{sock} = $osock;
	@{$conn->{lastDataTime}} = Time::HiRes::gettimeofday();		# start off with this marker for bw time
	$conn->blockReadLine();	# read server connection/response string
	if($conn->{user}){
		$conn->performLogin(1);
	}
}

sub getReconnectRemaining {
	my $conn = shift;
	return 0 unless defined($conn->{lastConnectRetryTime});
	return $conf->reconndur - (time - $conn->{lastConnectRetryTime});
}

sub _setLastConnectRetryTime {
	my $conn = shift;
	$conn->{lastConnectRetryTime} = time;
}

#########################################################################################
# Changes the current group for the connection, only if it's not already
# in a group that the file was posted to.  May issue multiple GROUP commands
# until one succeeds.
#########################################################################################
sub _maybeChangeGroup {
	my ($conn, $file) = @_;
	my $currentGroup = $conn->{currentGroup};
	my @groupList = @{$file->groups()};
	(grep(/$currentGroup/, @groupList)) and return 0; # already in group

	while(scalar @groupList > 0){
		my $group = shift @groupList;
		$conn->sockSend('GROUP ' . $group . "\r\n");
		my $line = $conn->blockReadLine();	# read server connection/response string
		if($line =~ /^2\d\d\s.*\r\n/s){		# good response, changed successful
			$conn->{currentGroup} = $group;
			statMsg(sprintf('Conn. #%d changed to group %s', $conn->number, $group));
			return 1;
		}
		statMsg("Group change failed: $line");
	}
	statMsg("Couldn't change to group required for this file...it will fail!");
	return 0;
}

sub _buildTempFilename {
	my ($conn, $segment) = @_;
	my $dir = $conf->getDestDirForFile($segment->parentFile());
	return sprintf('%s/nzbperl.%d.f%d.s%06d.part', $dir, $urunid, $segment->parentFile()->uid, $segment->number);
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  _   _         _               ____           _           _                 
# | \ | |  ____ | |__    _   _  |  _ \   _ __  (_)  _ __   | |_    ___   _ __ 
# |  \| | |_  / | '_ \  (_) (_) | |_) | | '__| | | | '_ \  | __|  / _ \ | '__|
# | |\  |  / /  | |_) |  _   _  |  __/  | |    | | | | | | | |_  |  __/ | |   
# |_| \_| /___| |_.__/  (_) (_) |_|     |_|    |_| |_| |_|  \__|  \___| |_|   
#
# Printer package used to contain static helpers for "printing"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
BEGIN {	
package Nzb::Printer;
use base 'Exporter';
our @EXPORT_OK = qw(statMsg porlp);
use strict;

#########################################################################################
# Adds a status message with timestamp
#########################################################################################
sub statMsg {
	$newStatMsg = 1;	# Set flag allowing repaint
	my $str = shift;
	my @t = localtime;
	my $msg = sprintf("%0.2d:%0.2d - %s", $t[2], $t[1], $str);
	my $logtimepart = sprintf("%d-%02d-%02d %0.2d:%0.2d:%0.2d", $t[5]+1900, $t[4]+1, $t[3], $t[2], $t[1], $t[0]);
	push @statusmsgs, $msg;
	if($conf->logfile){
		open LOGFH, ">>" . $conf->logfile or 
				(push @statusmsgs, sprintf("%0.2d:%0.2d:%0.2d - Error writing to log file  %s", $conf->logfile) and return 1); 
		print LOGFH sprintf("%s - %s\n", $logtimepart, $str);
		close LOGFH;
	}
	return 1;
}

#########################################################################################
# porlp :: "Print or log [with] param" - 
#########################################################################################
sub porlp {
	my $msg = shift;
	my $lognotprint = shift;
	if($lognotprint){
		chomp $msg;
		statMsg($msg);
	}
	else{
		print $msg;
	}
}
}# end of BEGIN block for Nzb::Printer package

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  _   _         _               _____   _   _        
# | \ | |  ____ | |__    _   _  |  ___| (_) | |   ___ 
# |  \| | |_  / | '_ \  (_) (_) | |_    | | | |  / _ \
# | |\  |  / /  | |_) |  _   _  |  _|   | | | | |  __/
# |_| \_| /___| |_.__/  (_) (_) |_|     |_| |_|  \___|
#
# Represents a single file from inside an nzb file.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
package Nzb::File;
use strict;
use IO::Socket::INET;
use fields;
my $unique_file_id = 0;
sub new {
	my ($class, $name, $date, $groups, $segments) = @_;

	my $self = {
		name => $name,
		parentNzb => undef,
		date => $date,
		groups => $groups,
		segments => defined($segments) ? $segments : [] ,
		trueFilename => undef,
		completedSegmentCount => 0,
		existsOnDisk => 0,
		bytesDownloaded => 0,
		skipped => 0,
		isBroken => 0
	};
	$self->{uid} = $unique_file_id++;
	bless($self, $class);
	foreach my $seg (@{$segments}){
		$seg->parentFile($self);
	}
	return $self;
}

sub name {
	return $_[0]->{name};
}

sub date {
	return $_[0]->{date};
}

sub groups {
	return $_[0]->{groups};
}

sub uid {
	return $_[0]->{uid};
}

sub segments {  # erg, leaky abs
	my $file = shift;
	return @{$file->{segments}};
}

sub segmentCount {
	my $file = shift;
	return scalar @{$file->{segments}};
}

sub parentNzb {
	my ($file, $nzbFile) = @_;
	return $file->{parentNzb} unless defined($nzbFile);	# get not set
	$file->{parentNzb} = $nzbFile;
}

sub skipped {
	my ($file, $skipval) = @_;
	return $file->{skipped} unless defined($skipval);
	$file->{skipped} = $skipval;
}

sub isBroken {
	my ($file, $val) = @_;
	return $file->{isBroken} unless defined($val);	# get not set
	$file->{isBroken} = $val;
}

sub bytesDownloaded {
	return $_[0]->{bytesDownloaded};
}

sub percentComplete {
	my $file = shift;
	return 0 unless ($file->totalSize > 0 and $file->{bytesDownloaded} > 0);
	my $perc = 100 * ($file->{bytesDownloaded} / $file->totalSize);
	return $perc > 100 ? 100 : $perc;
}

sub addDownloadedBytes {
	my ($file, $bytes) = @_;
	return $file->{bytesDownloaded} += $bytes;
}

# Returns sum of the size of all segments
sub totalSize {
	my $tot = 0;
	foreach my $seg (@{$_[0]->{segments}}){
		$tot += $seg->size;
	}
	return $tot;
}

sub trueFilename {
	my ($file, $trueName) = @_;
	return $file->{trueFilename} unless defined($trueName);	# get not set
	$file->{trueFilename} = $trueName;
}

sub existsOnDisk {
	my ($file, $val) = @_;
	return $file->{existsOnDisk} unless defined($val);	# get not set
	$file->{existsOnDisk} = $val;
}

#########################################################################################
# Not a true average, but an average of all segments except the last one...
# ...unless there's only one segment, in which case it's the segment size.
#########################################################################################
sub avgSegSize {
	my $file = shift;
	my @segs = @{$file->{segments}};
	return $segs[0]->size unless scalar @segs > 1;
	my ($sum, $ct) = (0, 0);
	foreach my $i (1..scalar(@segs)){
		my $seg = $segs[$i-1];
		last unless $i < scalar(@segs);
		$ct++;
		$sum += $seg->size;
	}
	return $sum*1.0/($ct*1.0);
}

# For now, we just keep a simple count so we can tell when all segments are done
sub markSegmentAsComplete {
	my ($file, $seg) = @_;
	$file->{completedSegmentCount}++;
}

sub allSegmentsAreFinished {
	my ($file) = @_;
	return ($file->{completedSegmentCount} == scalar(@{$file->{segments}}));
}

sub completedSegmentCount {
	my $file = shift;
	return $file->{completedSegmentCount};
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  _   _         _               ____                                              _   
# | \ | |  ____ | |__    _   _  / ___|    ___    __ _   _ __ ___     ___   _ __   | |_ 
# |  \| | |_  / | '_ \  (_) (_) \___ \   / _ \  / _` | | '_ ` _ \   / _ \ | '_ \  | __|
# | |\  |  / /  | |_) |  _   _   ___) | |  __/ | (_| | | | | | | | |  __/ | | | | | |_ 
# |_| \_| /___| |_.__/  (_) (_) |____/   \___|  \__, | |_| |_| |_|  \___| |_| |_|  \__|
#                                               |___/                                  
# Represents a single segment from an nzb.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
package Nzb::Segment;
use strict;
use fields;
sub new {
	my ($class, $msgid, $size, $number) = @_;
	my $self = {
		parentFile => undef, msgid => $msgid, size => $size, number => $number
	};
	bless($self, $class);
	return $self;
}

sub id {
	return $_[0]->{msgid};
}

sub size {
	return $_[0]->{size};
}

sub number {
	return $_[0]->{number};
}

sub parentFile {
	my ($seg, $file) = @_;
	return $seg->{parentFile} unless defined($file);	# get not set
	$seg->{parentFile} = $file;
}

#########################################################################################
# Just a delegate -- lets our parent file know that we're finished.
#########################################################################################
sub markAsComplete {
	my $seg = shift;
	$seg->{parentFile}->markSegmentAsComplete($seg) unless not defined $seg->{parentFile};
}
