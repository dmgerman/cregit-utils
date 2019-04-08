import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}
import scala.io.Source
import sys.process._

import scala.collection.convert.ImplicitConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import org.eclipse.jgit.api._
import org.eclipse.jgit._
import org.eclipse.jgit.treewalk.TreeWalk
import org.eclipse.jgit.lib.Config
import org.eclipse.jgit.lib
import org.eclipse.jgit.lib.Constants
import org.eclipse.jgit.storage.file.FileRepositoryBuilder;
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.revwalk.RevSort._
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.revwalk.RevSort._
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.transport.ReceiveCommand
import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.treewalk.TreeWalk;
import org.eclipse.jgit.lib.TreeFormatter
import org.eclipse.jgit.lib.FileMode
import org.eclipse.jgit.lib.CommitBuilder
import org.eclipse.jgit.lib.PersonIdent
import org.eclipse.jgit.revwalk.RevTree
import scala.collection.JavaConverters._
//import scala.collection.JavaConversions._
//import scala.collection.convert.wrapAsScala._
import org.eclipse.jgit.treewalk.filter.PathFilter
import org.eclipse.jgit.diff.RawTextComparator

import java.io.File
import scala.io.Source
import sys.process._

class File_Entry(val path: String, val fileMode: FileMode, val oid: ObjectId) {

}

class Blame_Entry(val commit: RevCommit, val line: Int, val filename: String) {

}

class Blame_File(val latestCommit: ObjectId, val fileName: String,
                 val firstCommit : ObjectId, val blameData: List[Blame_Entry],
                 val contents: Array[String])  {

  def get_line(line: Int) = {
    assert(blameData != null, "trying to retrieve line from empty BlameData")
    assert(blameData.size > line, s"trying to retrieve line [$line] when the size is [${blameData.size}] [$fileName] latestCommit[$latestCommit]")
    blameData(line)
  }

  def combine(other: Blame_File) : Blame_File = {
    // isDummyCommit indicates if joiningCommit was created to join the histories

    assert(firstCommit!=null)

    // make sure we have first commit
    val joiningCommit = other.latestCommit

    assert(joiningCommit!= null)

    val isDummyCommit = joiningCommit.getName != firstCommit.getName

    // combine the blames
    val newBlameData = blameData.map{blameEntry =>

      // if the commit is the first commit of the file
      // replace the blame info
      if (blameEntry.commit.getName == firstCommit.getName) {
        val otherCommit = other.get_line(blameEntry.line).commit
        // did we have to create an artificial commit?
        if (isDummyCommit && otherCommit.getName == joiningCommit.getName) {
          // it is due to the modification during copy. keep the attribution
          blameEntry
        } else {
          // attribute it to the other blame
          other.get_line(blameEntry.line)
        }
      } else {
        blameEntry
      }
    }
    // return a new Blame with
    // new blamedata, but same original info, except for a new
    // firstCommit (we use the one of the other blame)
    new Blame_File(latestCommit, fileName,
      other.firstCommit, newBlameData, contents)
  }

  def output = {
    // make sure we have as many lines in the contents as in the blame
    // otherwise something is terribly wrong
    assert(contents.length ==  blameData.length, s"Length of contents [${contents.length}] is different than blame data [${blameData.length}]")
    blameData.
//      take(10).
      zip(
        contents.zipWithIndex).
      foreach{ case (entry, (line, i)) =>
        println(s"${i+1} ${entry.commit.getName} ${entry.filename} (${entry.commit.getAuthorIdent}) ${line}")
    }
  }
}

class My_Repo (val repoDir: String) {

  val repo = open_repo
  val git = new Git(repo)

  def open_repo  = {
    val repoFile = new File(repoDir)
    // build the repo data structure
    val builder = new FileRepositoryBuilder()
    builder.findGitDir(repoFile)
    if (builder.getGitDir== null) {
      System.err.println(s"[$repoDir] is not a git repository")
      System.exit(1)
    }
    builder.build()
  }

  def traversable_commits () = {

    // get all commits reachable from any reference
    // in the repo

    val walk = new RevWalk(repo)

    val startCommits : List[RevCommit] =
      repo.
        getAllRefs.map(_._2).
        map{ref =>
          walk.parseAny(ref.getObjectId)
        }.
        collect { case c: RevCommit => c }.
        toList.
        distinct

    walk.sort(TOPO) // traverse from roots to heads
    walk.sort(REVERSE, true) // oldest first
    walk.markStart(startCommits)
    walk
  }

  def resolve (id: String) = {
    repo.resolve(id)
  }

  def commit_tree_contents(c: RevCommit) = {
    // returns an iterator of a pair of (path, objectId)

    val tree = c.getTree()

    val treeWalk = new TreeWalk(repo)
    treeWalk.addTree(tree)
    treeWalk.setRecursive(false )
    //treeWalk.setPostOrderTraversal(true)
    val iter = new Iterator[(String, Boolean, ObjectId)] {
      def next() = {
        val path = treeWalk.getPathString()
        val isSubtree = treeWalk.isSubtree
        val oid = treeWalk.getObjectId(0)
        if (isSubtree)
          treeWalk.enterSubtree
        (path, isSubtree, oid)
      }
      def hasNext () = treeWalk.next()
    }
    iter
  }

  def get_rev_commit(commitId: ObjectId) = {
    val revWalk = new RevWalk(repo);
    revWalk.parseCommit(commitId);
  }

  def blob_at_commit(commitid: ObjectId, path: String) = {

    val commit = get_rev_commit(commitid);

    val tree = commit.getTree();
    val treeWalk = new TreeWalk(repo);
    treeWalk.addTree(tree);
    treeWalk.setRecursive(true);
    treeWalk.setFilter(PathFilter.create(path));
    if (!treeWalk.next()) {
      System.err.println(s"we did not find file [$path] in commit " + commitid.getName)
      System.exit(1)
      null
    } else {
      treeWalk.getObjectId(0);
    }
  }

  def create_commit(file: File_Entry, parent: ObjectId) = {

    // we create a commit that simply updates file in the commit parent
    // which will become the parent of this new commit

    def replace_file_in_tree(tree: RevTree, file: File_Entry): ObjectId = {
      // builds a tree from another, but replaces the given file with new contents

      val objectInserter = repo.newObjectInserter();
      assert(objectInserter!=null)

      def rebuild_tree(treeOid: ObjectId, pathToReplace: String): ObjectId = {

        val treeWalk = new TreeWalk(repo)
        treeWalk.addTree(treeOid)
        treeWalk.setRecursive(false )
        val iter = new Iterator[(String, Boolean, FileMode, ObjectId)] {
          def next() = {
            val path = treeWalk.getPathString()
            val isSubtree = treeWalk.isSubtree
            val fileMode = treeWalk.getFileMode(0)
            val oid = treeWalk.getObjectId(0)
            (path, isSubtree, fileMode, oid)
          }
          def hasNext () = treeWalk.next()
        }
        val treeFormatter = new TreeFormatter();

        iter.map{ case (path, isSubtree, fileMode, oid) =>
          if (isSubtree && (pathToReplace contains (path + "/"))) {
            val newOid = rebuild_tree(oid, pathToReplace.substring(path.length+1))
            (path, fileMode, newOid)
          } else if(path == pathToReplace) {
            (path, file.fileMode, file.oid)
          } else {
            (path, fileMode, oid)
          }
        }.foreach{ case (path, filemode, oid) =>
            treeFormatter.append(path, filemode, oid)
        }
        // insert and return the new treeId
        objectInserter.insert( treeFormatter );
      }
      val id = rebuild_tree(tree, file.path)

      objectInserter.flush();
      id
    }

    val revParent = get_rev_commit(parent)
    assert(revParent != null)

    val treeId = replace_file_in_tree(revParent.getTree, file)

    val commitBuilder = new CommitBuilder()
    commitBuilder.setTreeId( treeId )
    commitBuilder.setMessage( s"Dummy commit to enhance history.\nreplaces file ${file.path}")
    commitBuilder.setParentId(revParent)

    val person = new PersonIdent( "me", "me@example.com" )
    commitBuilder.setAuthor( person )
    commitBuilder.setCommitter( person )

    val objectInserter2 = repo.newObjectInserter();
    val commitId = objectInserter2.insert( commitBuilder );
    objectInserter2.flush();

    commitId

  }

  def do_blame_file_jgit(latestCommit:ObjectId, firstCommit: ObjectId, path:String, contents: Array[String]): Blame_File = {
    val blame = git.blame()

    blame.setStartCommit(latestCommit)
      .setFilePath(path)
      .setFollowFileRenames(true)
      .setTextComparator(RawTextComparator.WS_IGNORE_ALL)

    val blameResult = blame.call()
    val blameText = blameResult.getResultContents()

    val size = blameText.size

    val listBlame = (0 to size-1).map { i =>
      val commit = blameResult.getSourceCommit(i)
      val line = blameResult.getSourceLine(i)
      val path = blameResult.getSourcePath(i);
      new Blame_Entry(commit, line, path)
    }

    new Blame_File(latestCommit, path, firstCommit, listBlame.toList, contents)
  }

  def do_blame_file_git(latestCommit:ObjectId, firstCommit: ObjectId, path:String, contents: Array[String]): Blame_File = {
    // call git-blame
    // process output
    val blameCommand = "git -C " + repoDir  + " blame --line-porcelain -w " + latestCommit.getName +" "  + path
    val lines = blameCommand !!

    val recordsBlame = lines.
      split("\n").
      foldLeft(List[List[String]]()) { case (acc, line) =>
        if (line.matches("^[0-9a-f]{40} .*$")) {
          val v = List[String](line)
          v :: acc
        } else {
          acc match {
            case (hd::tl) => ((line::hd) :: tl)
          }
        }
      }.reverse.map{_.reverse}

    //println(s"cid [${latestCommit}]size to process ${recordsBlame.size} -> {$path}")

    val blameLines = recordsBlame.map { lines =>
      val commitLine = lines(0)

      val prevFileNameLine = if (lines(10).startsWith("previous")) lines(11) else lines(10)
//      assert(prevFileName.startsWith("filename"))
      val commitPattern = "^([0-9a-f]{40}) ([0-9]+) .+$".r
//      println(commitLine)
      val commitPattern(cid, prevLineNumber) = commitLine
//      println("after")

      val prevFilePattern = "^filename (.+)$".r
      val prevFilePattern(prevFileName) = prevFileNameLine

      //println(s"Commit [$cid] line [$prevLineNumber] prevfile [$prevFileName]")


      val rvCommit = get_rev_commit(repo.resolve(cid))
      new Blame_Entry(rvCommit, prevLineNumber.toInt-1, prevFileName)
    }

    new Blame_File(latestCommit, path, firstCommit, blameLines, contents)

  }

  val do_blame_file = do_blame_file_git _

  //  def combine_blames()

  def first_commit(path:String) = {
    val log = git.log.addPath(path).call()
    val logIt = log.asScala.toIterator.toList
    logIt.last
  }

  def contents_at_commit(cid: ObjectId, path: String) = {
    val contentsBlobId = blob_at_commit(cid, path)
    val loader = repo.open(contentsBlobId)
    scala.io.Source.fromInputStream(loader.openStream).
      getLines.
      toArray
  }

}


class LogFollowRecord(
  val fileName: String,
  val cid: String,
  val operation: String,
  val prevFileName: String,
  val nextFileName: String,
  val similarity :String,
  val plus: String, val minus: String) {

  def mkString = {
    fileName + "; " + cid + "; " + operation + "; prevName: " + prevFileName + "; " + nextFileName + "; " + similarity + "; " + plus + ":" + minus
  }
}

class My_Logger(dir: String) {

  def split_records(lines:String, separator: String) : List[List[String]] = {

    def containsCopyRename(record: List[String]) = {
      record match {
        case (comLine::statsLine::_::_::sim::act::_) =>
          (act.startsWith("copy from") || act.startsWith("rename from"))
        case _ => false
      }
    }

    def containsCopy(record: List[String]) = {
      record match {
        case (comLine::statsLine::_::_::sim::act::_) =>
          (act.startsWith("copy from"))
        case _ => false
      }
    }

    val recordsRev = lines.
      split("\n").
      foldLeft(List[List[String]]()) { case (acc, line) =>
        if (line.startsWith(separator)) {
          val v = List[String](line)
          v :: acc
        } else {
          acc match {
            case (hd::tl) => ((line::hd) :: tl)
          }
        }
      }
    val records = recordsRev.reverse.map(_.reverse).filter(containsCopyRename(_))
    //val records = recordsRev.reverse.map(_.reverse).filter(containsCopy(_))
    records
  }

  def parse_record(record: List[String]): LogFollowRecord = {

    def parse_operation(line:String, dest:String) = {
      val position = line.indexOfSlice(" " + dest + " ")
      assert(position >= 0)
      val op = line.substring(0, position)
      val filename = line.substring( position+ dest.length + 2)

      (op, filename)
    }
    def parse_operation_to(line:String) = {
      val position = line.indexOfSlice(" from ")
      assert(position >= 0)
      val op = line.substring(0, position)
      val filename = line.substring( position+ 6)

      (op, filename)
    }

    def parse_similarity(line:String) = {
      val position = line.indexOfSlice("similarity index")
      assert(position>=0, s"Not a valid similarity line [$line]")
      line.substring(position+16)
    }

    def parse_comLine(line:String) = {
      val pattern = ">>;([^;]+);([0-9a-f]{40})$".r
      val pattern(filename, cid) = line
      (filename, cid)
    }
    def parse_plus_minus(line:String) = {
      val pattern = "^([0-9-]+)\t+([0-9-]+).+$".r
      val pattern(minus,plus) = line
      (minus, plus)
    }


    assert(record.length >= 6, s"not a valid record ${record.mkString}")
    assert(record(0).startsWith(">>;"))

    record match {
      case (comLine::statsLine::_::_::sim::operation::operationTo::_) =>
        val (filename, cid) = parse_comLine(comLine)
        val (plus, minus) = parse_plus_minus(statsLine)
        val simIndex = parse_similarity(sim)
        val (op, prevFileName) = parse_operation(operation, "from")
        val (op2, nextFileName) = parse_operation(operationTo, "to")
        assert(op == op2, s"operations are different [$op][$op2]")
        new LogFollowRecord(filename, cid, op, prevFileName, nextFileName, simIndex, plus, minus)
    }

  }

  def logFollow (fileName:String):List[LogFollowRecord] = {
    val quote = "'"
    val logCommand = s"git -C $dir log -p -w --follow --ignore-all-space --numstat --pretty=format:>>;$fileName;%H $fileName"
    System.err.println("running " + logCommand)
    val log = logCommand !!
    val revRecords = split_records(log, ">>;")
    val result = revRecords.map( r => parse_record(r))
    result
  }
}


object Main extends App {

  def Blame(records: List[LogFollowRecord], originalBlame: Blame_File):Blame_File = {

    if (records.size == 0) {
      originalBlame
    } else {
      val record = records.head
      //println(s"-------------------------------------------------------from[${record.prevFileName}] to[${record.nextFileName}] cid[${record.cid}]\n")
      //println(record.mkString)

      val ansCommitId = repo.resolve(record.cid);
      val firstVersionBlob = repo.blob_at_commit(ansCommitId, record.nextFileName)
      val ansFileBlob = repo.blob_at_commit(ansCommitId, record.prevFileName)

      //println("++++++++++++++++")

      assert(ansFileBlob != null)

      val commitToBlame =
        if (firstVersionBlob != ansFileBlob) {
          // create dummy commit with ancestor commit as parent
          // and the contents of the firstCommit of the file we are tracking
          val fileToReplace = new File_Entry(record.prevFileName, FileMode.REGULAR_FILE, firstVersionBlob)
          repo.create_commit(fileToReplace, ansCommitId)
        } else {
          // othewise we just blame from the ancestor latest commit
          ansCommitId
        }

      val secondBlame = repo.do_blame_file(commitToBlame, ansCommitId, record.prevFileName, Array[String]())

      val newBlame = originalBlame.combine(secondBlame)

      Blame(records.tail, newBlame)
    }
  }


  def Usage(st:String) {
    System.err.println("Usage <repoDir> <filename\n")
    System.err.println(st)
    System.exit(1)
  }

  if (args.size != 2) {
    Usage("")
  }


  //val repoDir = "/tmp/token/"
  val repoDir = args(0)
  //val fileName = "arch/alpha/boot/misc.c"
//  val fileName = "arch/alpha/boot/bootp.c"
  //val fileName = "tools/lib/hweight.c"
  //val fileName = "tools/testing/selftests/rseq/rseq-mips.h"
  //val fileName = "tools/testing/selftests/timers/nsleep-lat.c"
  //val fileName = "arch/alpha/kernel/core_marvel.c"
  val fileName = args(1)

  val logger = new My_Logger(repoDir)

  val logRecords = logger.logFollow(fileName).filter(rec => rec.operation == "copy")

  logRecords.foreach(r=>println(r.mkString))

  val repo = new My_Repo(repoDir)


  val headId = repo.resolve("HEAD");

  if (headId == null) {
    Usage(s"HEAD did not exit in repo");
  }

  val contents = repo.contents_at_commit(headId, fileName)
  if (contents == null) {
    Usage(s"File ${fileName} does not exist in commit [$headId] in repo");
  }
  //println(contents(0))

  assert(logRecords.length > 0)
  val firstCid = repo.resolve(logRecords.head.cid)
  val originalBlame = repo.do_blame_file(headId, firstCid, fileName, contents)

  val result = Blame(logRecords, originalBlame)

  result.output

}
