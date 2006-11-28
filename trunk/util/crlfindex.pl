#!/usr/bin/perl
#

$str = "this is a\r\n.\r\ntest to see what this does\n";
if($str =~ /\r\n\.\r\n/s){
	print "YES\n";
}
else{
	print "NO";
}


$ind = index $str, "\r\n";
printf "Found at $ind\n";
$line = substr($str, 0, $ind+2, '');
print "line is '$line'\n";
print "now string is '$str'\n";

$str = ".\r\n";
if($str =~ /^\.\r\n/){
	print "YES\n";
}
else{
	print "NO\n";
}

$x = 0;
print "Before: $x";
$x = $x ^ 0x01;
print "After: $x";
