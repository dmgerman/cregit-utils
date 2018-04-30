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
use Pod::Usage;
use Getopt::Long;
use File::Basename;

my $commandPath = dirname(__FILE__);


my $extension = ".token";

my $thisCommand = "/home/dmg/git.dmg/cregit-scala/tokenize/tokenizeSrcMl.pl";

my $help = 0;
my $man = 0;
my $overwrite = 0;
my $verbose = 0;

GetOptions ("command=s" => \$thisCommand,
            "help"     => \$help,      # string
            "extension=s" => \$extension,
            "overwrite"         => \$overwrite,
            "verbose"           => \$verbose,
            "man"      => \$man)   # flag
        or die("Error in command line arguments\n");

my @command = ($thisCommand, "--position");

if ($man) {
    pod2usage(-verbose=>2);
    exit(1);
}

if (scalar(@ARGV) != 3) {
    pod2usage(-verbose=>1);
    exit(1);
}

my $repoDir = shift;
my $outputDir = shift;
my $fileRegExpr = shift;


open(FILES, "git -C '$repoDir' ls-files|") or die "unable to traverse git repo [$repoDir] $!";

my $count = 0;
my $alreadyDone = 0;
my $errorCount = 0;


while (<FILES>) {
#    next unless /^kernel/;
    chomp;

    if ($fileRegExpr ne "") {
        next unless /$fileRegExpr/;
    }

    my $name = $_;
    
    if ($verbose) {
        print STDERR ("matched file: [$name]\n");
    }

    my $originalFile = $repoDir . "/" . $name;
    my $outputFile = $outputDir . "/" . $name . $extension;

    next unless -f $originalFile;

    next unless (-f $originalFile > 0);

    if (!$overwrite && -f $outputFile) {
        $alreadyDone ++;
        next;
    }
    $count++;
    print STDERR ("$count: $name\n") if $verbose;

    my ($fh, $temp) = mkstemp( "tmpfile-XXXXX" );
    close($fh);

    my $errorCode = execute_command(@command, "$repoDir/$name", $temp);
    if ($errorCode != 0) {
        print "Error code [$errorCode][$name]\n";
        $errorCount ++;
    } else {
        # command already moves file
        move_file($temp, "$outputFile");
    }
}


print "Newly processed [$count] Already done [$alreadyDone] files Error [$errorCount]\n";

sub move_file
{
    my ($from, $to) = @_;
    
    my $toDir = dirname($to);

    printf ("copy [$from] to [$to] in dir [$toDir]\n") if $verbose;

    die "from file does not exist in copy_file [$from]" if not -f $from;

    if (not -d $toDir) {
        printf("Creating directory [$toDir]\n") if $verbose;
	make_path($toDir) or "die unable to create directory $to";
    } 
    move($from, $to) or
            (unlink($to),  "unable to move [$from] to [$to]");
}


sub Usage {
    my ($m) = @_;
    print STDERR "$m\n";
    pod2usage(-verbose=>1);
    exit(1);
}


sub execute_command {
    my (@command) = @_;
    # make sure we have more than one element in the array
    #    otherwise system will use the shell to do the execution
    if ($verbose) {
        print STDERR join(' ',@command), "\n";
    }

    die "command (@command) seems to be missing parameters" unless (scalar(@command) > 1);

    my $status = system(@command);

    return $status;
}

__END__

=head1 NAME

blameForwardRepo.pl: create the forward "blame" of files in a git repository

=head1 SYNOPSIS

  xxxx [options] <repository> <outputDirectory> <fileNameRegexp>

     Options:
       --override         overwrite existing files
       --help             brief help message
       --man              full documentation
       --blameExtension=s extension to use in blame
       --formatblame=s    full path formatBlame (command to create them blame).
                          By default it looks in the same directory as this script,
                          otherwise it will try to execute the one in the PATH

=head1 OPTIONS

=over 8

=item B<--help>

    Print a brief help message and exits.

=item B<--man>

    Prints the manual page and exits.

=item B<--override>

    By default, if an output file exists it is skipped. This changes that behaviour.

=back

=head1 DESCRIPTION

B<This program> will read the given input file(s) and do something
useful with the contents thereof.

=cut
