#!/usr/bin/perl

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


use File::Basename;
use File::Path qw(make_path remove_tree);
use strict;
use File::Temp qw/ tempfile tempdir mkstemp/;
use File::Copy;
use Getopt::Long;
use Pod::Usage;

my $blameExtension = ".blame";
my $help = 0;
my $man = 0;
my $verbose = 0;

GetOptions ("blameExtension=s" => \$blameExtension, 
            "help"     => \$help,      # string
            "verbose"  => \$verbose,
            "man"      => \$man)   # flag
        or die("Error in command line arguments\n");

if ($man) {
    pod2usage(-verbose=>2);
    exit(1)
}

if (scalar(@ARGV) != 5) {
    pod2usage(-verbose=>1);
    exit(1)
}

my $baseCid = shift @ARGV;
my $futureCid = shift @ARGV;
my $repo = shift @ARGV;
my $file = shift @ARGV;
my $dest = shift @ARGV;

my ($fh, $temp) = mkstemp( "tmpfile-XXXXX" );


Usage("Error [$file] should be a file in repository [$repo] [$repo/$file]\n\n usage $0 <repo> <filename> <destinationDir>") unless -f "$repo/$file";
Usage( "Error [$repo] should be a git repo\n\nUsage $0 <repo> <filename> <destinationDir>") unless -d "$repo/.git";
Usage( "Error [$dest] should be a directory\n\nUsage $0 <repo> <filename> <destinationDir>") unless -d $dest and $dest ne "";

#Usage("Error [$baseCid] does not look like a commit ") unless looks_like_commit($baseCid);
#Usage("Error [$futureCid] does not look like a commit ") unless looks_like_commit($futureCid);

if (not looks_like_commit($baseCid)) {
    $baseCid = Remap_Commit($baseCid, $repo);
}

if (not looks_like_commit($futureCid)) {
    $futureCid = Remap_Commit($futureCid, $repo);
}



if ($verbose) {
    print STDERR "$0 processing repo [$repo] file [$file] [$dest]\n";
}

open(IN, "git -C '$repo' blame -w -n --reverse --line-porcelain $baseCid..$futureCid '$file'|" ) or "unable to execute git ";

while (my $l = Read_Record()) {
    print $fh $l;
    print $fh "\n";
}
close IN;
close $fh;
copy_file($temp, $dest, $file . $blameExtension);
if ($verbose) {
    print STDERR "...completed\n";
}


sub Read_Record {
    my $f ;
    my $cid;

    while (<IN>) {
#        print ;
	chomp;
        if ($_ =~ /^([0-9a-f]{40}) ([0-9]+) ([0-9]+)/ ) {
            $cid = $1;
            my $lineOrigin = $3;
            my $lineFinal = $2;

            $f = $lineOrigin . ";";
            
            if ($cid ne $futureCid) {
                $f .= "changed/deleted;;";
            } else {
                $f .= ";$lineFinal;"
            }
            
        } elsif ($_ =~ /^(filename) (.+)$/) {
            $f .=  $2 . ";";
	} elsif (/^	(.*)/) { #actual line
	    $f = $f . $_;
	    return $f;
        } elsif (/^([0-9a-f]{40})/) {
            print STDERR $_;
            die "Something wrong. this is a commit beginning we should have seen";
	} else {
	    ; # simply ignore
	}
    }
    return $f;
}

sub Usage {
    my ($m) = @_;
    print STDERR "$m\n";
    pod2usage(-verbose=>1);
    exit(1);
}

sub copy_file
{
    my ($from, $toDir, $toName) = @_;
    
    my $to = "${toDir}/$toName";
    $toDir = dirname($to);

#    printf ("copy [$from] to [$to] [$toDir][$toName]\n");

    die "from file does not exist in copy_file [$from]" if not -f $from;

    if (not -d $toDir) {
        printf("Creating directory [$toDir]\n") if $verbose;
	make_path($toDir) or "die unable to create directory $to";
    } 
    move($from, $to) or
            (unlink($to),  "unable to move [$from] to [$to]");
}

sub looks_like_commit {
    my ($cid) = @_;
    return ($cid =~ /^[a-f0-9]{40}$/i);
}

sub Remap_Commit {
    my ($ref, $repo) = @_;
    my $cid = `git -C $repo rev-list -n 1 '$ref'`;
    chomp $cid;
    Usage("Error [$ref][$cid] does not look like a commit and it is not a ref ") unless looks_like_commit($cid);
    return $cid;
}
        

__END__

=head1 NAME

formatBlameForward.pl - extract the blame information for a file to a future revision

=head1 SYNOPSIS

formatForwardBlame.pl [options] <baseCid> <futureCid> <repository> <file> <outputDirectory>

     Options:
       --blame-extension=s  extension to append to output files. 
                            Default is .blame
       --help               brief help message
       --man                full documentation

=head1 OPTIONS

=over 8

=item B<--help>
    Print a brief help message and exits.

=item B<--man>
    Prints the manual page and exits.

=back

=head1 DESCRIPTION

B<This program> will read run blame on a given file in a given git 
repository and create a file in the output directory
with the same full path as the original file, with 
the .blame extension (which can be changed with the --blame option).
Blame is run in reverse, towards a future revision

=cut

