//import scala.concurrent.ExecutionContext.Implicits.global
//import scala.concurrent.duration.Duration
//import scala.util.{Failure, Success}
import scala.io.Source
import sys.process._

import org.eclipse.jgit.api._
//import org.eclipse.jgit._
//import org.eclipse.jgit.treewalk.TreeWalk
//import org.eclipse.jgit.lib.Config
//import org.eclipse.jgit.lib
//import org.eclipse.jgit.lib.Constants
//import org.eclipse.jgit.storage.file.FileRepositoryBuilder;
//import org.eclipse.jgit.lib.Repository
//import org.eclipse.jgit.revwalk.RevCommit;
//import org.eclipse.jgit.revwalk.RevWalk;
//import org.eclipse.jgit.revwalk.RevSort._
import org.eclipse.jgit.lib.ObjectId
//import org.eclipse.jgit.revwalk.RevSort._
//import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
//import org.eclipse.jgit.transport.ReceiveCommand
import org.eclipse.jgit.revwalk.RevCommit
//import org.eclipse.jgit.treewalk.TreeWalk;
//import org.eclipse.jgit.lib.TreeFormatter
import org.eclipse.jgit.lib.FileMode
//import org.eclipse.jgit.lib.CommitBuilder
//import org.eclipse.jgit.lib.PersonIdent
//import org.eclipse.jgit.revwalk.RevTree
//import scala.collection.JavaConverters._
//import scala.collection.JavaConversions._
//import scala.collection.convert.wrapAsScala._
//import org.eclipse.jgit.treewalk.filter.PathFilter
//import org.eclipse.jgit.diff.RawTextComparator






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

  if (logRecords.length > 0) {

    val firstCid = repo.resolve(logRecords.head.cid)
    val originalBlame = repo.do_blame_file(headId, headId, fileName, contents)
    val result = Blame(logRecords, originalBlame)
    result.output
  } else {
    System.err.println("This file [$fileName] has not being copied. Use git-blame instead")
  }

}
