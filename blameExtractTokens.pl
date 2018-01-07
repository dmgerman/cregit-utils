#!/usr/bin/perl

use strict;

my $filename = shift @ARGV;

die "Usage $0 <filename>" unless -f $filename;

open(IN, $filename) || die "unable top open [$filename]";

while (<IN>) {
    chomp;
    if (/^([a-f0-9]{40});;\s+([a-z]+)\|(.*)$/) {
        my $rest = $3;
        $rest =~ s/;/<SEMICOLON>/g;
        print("$filename;$1;$2;$rest\n")
   }
}
