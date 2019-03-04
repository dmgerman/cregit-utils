#!/usr/bin/perl

# how it is being run

# perl /home/dmg/git.dmg/cregit-scala/blameRepo/blameExtractTokensDB.pl /tmp/line.db /home/linux/4.18/blame.line "" 

use strict;

use DBI;

my $dbFile = shift;
my $prefix = shift;
my $name = shift;

my $filename = "$prefix/$name";

Usage() unless defined($filename) and $filename ne "/";
Usage() unless -f $filename or -d $filename;


my $dbh = DBI->connect("dbi:SQLite:dbname=$dbFile", "","",{
   PrintError       => 0,
   RaiseError       => 1,
   AutoCommit       => 0,
});

Create_Table_If_Needed("blameline", "create table blameline(
       filename     TEXT,
       cid          CHAR(40),
       line         int,
       contents     TEXT
    );");


my $ins = $dbh->prepare("insert into blameline(filename, cid, line, contents) values (?, ?, ?, ?);");

if (-f $filename) {
    # it is a file
    Load_File ($filename, $name);
} elsif (-d $filename) {
    # it is a directory

    my $dir = "$prefix/$name";

    my $count = 0;

    open(FILES, "find '$dir' -type f | ") or die "Unable to run 'find $prefix -type f'";


    while (<FILES>) {
        chomp;
        my $thisFile = $_;
        if (/\.blame$/) {
            my $file = substr($_, length($prefix)+1);
            $file =~ s/\.blame$//;
            Load_File($thisFile, $file);
            $count++;
        }
        print(">$count;$thisFile\n");
    }
    close(FILES);
}

$dbh->disconnect();
exit(0);

sub Load_File {

    my ($filename, $name) = @_;

    open(IN, $filename) || die "unable top open [$filename]";

    $name =~ s/\.blame$//;

    my $lineNo = 1;
    my $inserted = 0;
    my $insertedDecl = 0;
    my $insertedMarker = 0;
    while (<IN>) {
        chomp;

        my $original = $_;

        # split by ;
        my @fields = split(';');
        my $cid = shift @fields;
        my $oldnames = shift @fields;

        my $line = substr($original, length($cid . $oldnames)+2);

        $line =~ s/^\s+//;
        $ins->execute($name, $cid, $lineNo, $line);

        $lineNo++;
    }
    close(IN);
    print("Filename: $filename;$lineNo;$inserted;$insertedDecl;$insertedMarker\n");
    $dbh->commit();
}




sub Create_Table_If_Needed {
    my ($table, $createSQL) = @_;
    
    my ($exists) = Simple_Query("SELECT count(*) FROM sqlite_master WHERE type='table' AND name='$table';");

    if ($exists == 0) {
        Do_SQL($createSQL);
    }
}

sub Do_SQL {
    my ($command, @parms) = @_;
    my $sth = $dbh->prepare($command);
    $sth->execute(@parms);
}

sub Usage {
    die "Usage $0 <dbFile> <filename> <prefix> <name>";
}

sub Simple_Query {
   my ($query, @parms) = @_;
   my $q = $dbh->prepare($query);
   $q->execute(@parms);
   return $q->fetchrow();
}

