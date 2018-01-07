#!/usr/bin/perl

use strict;

use DateTime;
use DBI;

my $date = substr(DateTime->today,0,10);


my $type = shift @ARGV;
my $dbFile = shift @ARGV;
my $toProcess = shift @ARGV;
my $removePrefix = shift @ARGV;

my %memoFiles;
my %memoCids;

Usage("type must be 'old' or 'new'") unless $type eq "old" or $type eq "new";


Usage("database file [$dbFile] does not exist\n") unless -f $dbFile;

Usage("[$toProcess] is not something we can process (must be file or directory)") unless (-d $toProcess or -f $toProcess);

if (not substr($toProcess, 0, length($removePrefix)) eq $removePrefix) {
    Usage("second parameter [$removePrefix] should be a prefix of first parameter [$toProcess]");
}

# connect to database

my $dbh = DBI->connect("dbi:SQLite:dbname=$dbFile", "","",{
   PrintError       => 0,
   RaiseError       => 1,
   AutoCommit       => 0,
});

Create_Table_If_Needed("${type}tokentoline", "create table ${type}tokentoline(
       ${type}filename     TEXT,
       ${type}token        INT,
       ${type}line         TEXT,
       ${type}column      INT,
       primary key(${type}filename, ${type}token)
    );");

my $insPos = $dbh->prepare("insert into ${type}tokentoline(${type}filename, ${type}token, ${type}line, ${type}column) values (?, ?, ?, ?);");

my $i = 0;

if (-f $toProcess) {
    Do_File($toProcess, $removePrefix);
    $i++;
} else {

    Do_Directory($toProcess, $removePrefix);
}

print("Processed: added $i files\n");
# process ids

$dbh->commit();

$dbh->disconnect();

sub Do_Directory {
    my ($toProcess, $removePrefix) = @_;
    $toProcess =~ s@/$@@;
    my $dh;
    opendir($dh, $toProcess) || die "Can't open $toProcess: $!";
    while (readdir $dh) {
        my $baseName = $_;
        my $fileName = $toProcess . '/' . $_;
        print STDERR "doing $fileName ...\n";
        if (-f $fileName and $fileName =~ /\.token$/) {
            Do_File($fileName, $removePrefix);
            $i++;
        } elsif (-d $fileName){
            if ($baseName eq "." or $baseName eq "..") {
                ; # do nothing
            } else {
                Do_Directory($fileName, $removePrefix);
            }
        }
    }
   closedir $dh;
}

sub Usage {
    my (@message) = @_;
    print(@message);

    die "\nUsage $0 (old|new) <dbFile> <tablename> <directory|file>";
}

sub Do_SQL {
    my ($command, @parms) = @_;
    my $sth = $dbh->prepare($command);
    $sth->execute(@parms);
}


sub Do_File{
    my ($file, $prefix) = @_;

    my $repoFileName = substr($file, length($prefix));
    $repoFileName=~ s/\.token$//;

    my ($done) = Simple_Query("select count(*) from ${type}tokentoline where ${type}filename = ?", $repoFileName);
    if ($done > 0) {
        print STDERR "File [$repoFileName] ($file) has been already done \n";
        return;
    }

    open(IN, $file) or die "Unable to open [$file]";

    my $line = 1;
    while (<IN>) {
        my ($lines) = split('\|', $_);
        my ($row, $col) = split(':', $lines);

        undef($row) if ($row eq "" or $row eq "-");
        undef($col) if ($col eq "" or $col eq "-");
        
        if (defined($row) and defined($col)) {
            $insPos->execute($repoFileName, $line, $row, $col);
        }
        $line++;
    }
}

sub Simple_Query {
   my ($query, @parms) = @_;
   my $q = $dbh->prepare($query);
   $q->execute(@parms);
   return $q->fetchrow();
}


sub Create_Table_If_Needed {
    my ($table, $createSQL) = @_;
    
    my ($exists) = Simple_Query("SELECT count(*) FROM sqlite_master WHERE type='table' AND name='$table';");

    if ($exists == 0) {
        Do_SQL($createSQL);
    }
}
