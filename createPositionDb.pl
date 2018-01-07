#!/usr/bin/perl

use strict;

use DateTime;
use DBI;

my $date = substr(DateTime->today,0,10);


my $dbFile = shift @ARGV;
my $tableName = shift @ARGV;
my $toProcess = shift @ARGV;
my $removePrefix = shift @ARGV;

my %memoFiles;
my %memoCids;


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

my $lineField ;
if ($tableName eq "lines") {
    $lineField = "line";
} elsif ($tableName eq "tokens"){
    $lineField = "token";
} else {
    Usage("tablename must be (tokens|lines) [$tableName]");
}

Create_Table_If_Needed($tableName, "create table $tableName(
       oldfilename     TEXT,
       old${lineField} INT,
       newfilename     TEXT,
       new${lineField}  INT,
       ${lineField}modified     BOOLEAN,
       primary key(oldfilename, old${lineField})
    );");

my $insPos = $dbh->prepare("insert into $tableName(oldfilename, old${lineField}, newfilename, new${lineField}, ${lineField}modified) values (?, ?, ?, ?, ?);");

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
        if (-f $fileName and $fileName =~ /\.blame$/) {
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

    die "\nUsage $0 <dbFile> <tablename> <directory|file>";
}

sub Do_SQL {
    my ($command, @parms) = @_;
    my $sth = $dbh->prepare($command);
    $sth->execute(@parms);
}


sub Do_File{
    my ($file, $prefix) = @_;

    my $repoFileName = substr($file, length($prefix));
    $repoFileName=~ s/\.blame$//;

    my ($done) = Simple_Query("select count(*) from $tableName where oldfilename = ?", $repoFileName);
    if ($done > 0) {
        print STDERR "File [$repoFileName] ($file) has been already done \n";
        return;
    }

    open(IN, $file) or die "Unable to open [$file]";

    my $line = 1;
    while (<IN>) {
        my ($line, $changed, $newLine, $newFile) = split(';', $_);
        undef($newFile) if ($newFile eq "");
        undef($newLine) if ($newLine eq "");

        
        my $modified = ($changed ne "");
        $insPos->execute($repoFileName, $line, $newFile, $newLine, $modified);

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
