#!/usr/bin/perl

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

Create_Table_If_Needed("blametoken", "create table blametoken(
       filename     TEXT,
       cid          CHAR(40),
       line         int,
       ttype        TEXT,
       ttoken       TEXT
    );");

Create_Table_If_Needed("blamedecl", "create table blamedecl(
       filename     TEXT,
       cid          CHAR(40),
       line         int,
       decl         TEXT,
       value        TEXT
    );");

Create_Table_If_Needed("blamemarker", "create table blamemarker(
       filename     TEXT,
       cid          CHAR(40),
       line         int,
       marker       TEXT,
       value        TEXT
    );");



my $ins = $dbh->prepare("insert into blametoken(filename, cid, line, ttype, ttoken) values (?, ?, ?, ?, ?);");
my $insDecl = $dbh->prepare("insert into blamedecl(filename, cid, line, decl, value) values (?, ?, ?, ?, ?);");
my $insMarker = $dbh->prepare("insert into blamemarker(filename, cid, line, marker) values (?, ?, ?, ?);");

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
    while (<IN>) {
        chomp;

        my $original = $_;

        # split by ;
        my @fields = split(';');
        my $cid = shift @fields;
        my $oldnames = shift @fields;

        my $line = substr($original, length($cid . $oldnames)+2);
        
        $line =~ s/^\s+//;
        my $toSplit = index($line, '|');
        if ($toSplit > 0) {
            my $type = substr($line, 0, $toSplit);
            my $rest = substr($line, $toSplit+1);
            if ($type eq "DECL" or $type =~ /^(begin_|end_)/) {
#                print("Ignored [$_] [$type];[$rest]\n");
            } else {
                $rest =~ s/;/<SEMICOLON>/g;
                $ins->execute($name, $cid, $lineNo, $type, $rest);
                $inserted++;
            }
        } else {
            print("ignored [$_][$line]\n") if $line ne "" and $line !~ /^(begin_|end_)/;
        }
        $lineNo++;
    }
    close(IN);
    print("Filename: $filename;$lineNo;$inserted\n");
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

