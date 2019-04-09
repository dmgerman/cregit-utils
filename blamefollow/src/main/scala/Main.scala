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


  def Usage(st:String) {
    System.err.println("Usage <repoDir> <filename\n")
    System.err.println(st)
    System.exit(1)
  }

  if (args.size < 2 || args.size > 4) {
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
  val fromCid = if (args.size ==3) args(2) else "HEAD"

  val logger = new My_Logger(repoDir)


  val repo = new My_Repo(repoDir)


  def Blame(records: List[LogFollowRecord]):Blame_File = {

    def Blame_With_Dummy_Commit(record:LogFollowRecord, ansCommitId: ObjectId, firstId: ObjectId, contents: Array[String]) = {


        //println(s"-------------------------------------------------------from[${record.prevFileName}] to[${record.nextFileName}] cid[${record.cid}]\n")
        //println(record.mkString)
      val firstVersionBlob = repo.blob_at_commit(ansCommitId, record.nextFileName)
      val ansFileBlob = repo.blob_at_commit(ansCommitId, record.prevFileName)

      assert(firstVersionBlob != null, s"first blob for file does not exist at commit [$ansCommitId] blob [$firstVersionBlob]")

      assert(ansFileBlob != null, s"ancestor blob for file does not exist at commit [$ansCommitId] blob [$ansFileBlob]")

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

      val newBlame = repo.do_blame_file(commitToBlame, firstId, record.prevFileName, contents)
      System.err.println(s"____________________________ $commitToBlame")

      newBlame
    }


    // we need to fold to keep track of the previous firstCommit.
    // we invert to start backwards. HEAD is just a placeholder to avoid errors, it is not used
    // as the last element does not have an ancestor

    val blamesFolding = records.
      reverse.
      foldLeft((List[Blame_File](), repo.resolve("HEAD"))){ case ((acc, firstId), record) =>


        val ansCommitId = repo.resolve(record.cid);

        val newBlame =
          if (record.operation == "copy") {
            val contents = repo.contents_at_commit(ansCommitId, record.nextFileName)

            System.err.println(s"blaming copy")
            Blame_With_Dummy_Commit(record, ansCommitId, firstId, contents)
          } else {
            val contents = repo.contents_at_commit(ansCommitId, record.fileName)

            System.err.println(s"blaming original [$ansCommitId]")
            repo.do_blame_file(ansCommitId, firstId, record.fileName, contents)
          }

        (newBlame::acc, ansCommitId)
      }

    // we don't need the firstCommit of the last one, so we throw it away
    val blames = blamesFolding._1.reverse

    val firstBlame = blames.head
    val tailBlame = blames.tail

    tailBlame.foldLeft(firstBlame:Blame_File){ (acc, blame) =>

/*
      System.err.println(s"->combining...")
      System.err.println(s"  this blame ${blame.fileName} latest ${blame.latestCommit} first ${blame.firstCommit}")
//      blame.output_n(5)
      System.err.println(s"  acc ${acc.fileName} latest ${acc.latestCommit} first ${acc.firstCommit}")
      //acc.output_n(5)
      System.err.println("____________________________")
 */

      blame.combine(acc)
    }

  }




  val startCid = repo.resolve(fromCid);

  System.err.println(s"Logging from $fromCid")

  if (startCid == null) {
    Usage(s"$startCid does not exit in repo [$repoDir]");
  }

  val logRecords = logger.logFollow(fileName, fromCid).
    filter(rec => rec.operation == "copy" || rec.operation == "original")

  logRecords.foreach(r=>System.err.println(r.mkString))

    //println(contents(0))

  val result = Blame(logRecords)
  result.output

}
