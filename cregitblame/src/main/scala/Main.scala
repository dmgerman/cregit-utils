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

class File_Entry(var path: String, var fileMode: FileMode, var oid: ObjectId) {

}

object Main extends App {

  // arguments:
  //   repoDir
//    val repoDir = "/home/linux/original.repo/linux-all-grafted/"
//  val repoDir = "/home/dmg/git.w/git-stein"
//  val repoDir = "/tmp/git"
  val repoDir = "/tmp/linux"
//  val repoDir = "/tmp/token"

////////////////////////////////////////////////////////
  val repoFile = new File(repoDir)
// build the repo data structure
  val builder = new FileRepositoryBuilder()
  builder.findGitDir(repoFile)
  val repo = builder.build()
  val git = new Git(repo)

  def traversable_commits (repo:Repository) = {

    // get all commits reachable from any reference
    // in the repo

    val walk = new RevWalk(repo)

    val startCommits : List[RevCommit] = repo.
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

//  val commits = traversable_commits(repo)

//  println(treeWalk.getTree.getClass.getSimpleName)

  def commits_contents(repo:Repository, c: RevCommit) = {
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
      println(s"we did not find [$path] in " + commitid)
      System.exit(1)
      null
    } else {
      treeWalk.getObjectId(0);
    }
  }


  def create_commit(repo: Repository, file: File_Entry, parent: RevCommit) = {

    def replace_file_in_tree(repo:Repository, tree: RevTree, file: File_Entry): ObjectId = {
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

    val treeId = replace_file_in_tree(repo, parent.getTree, file)

    val commitBuilder = new CommitBuilder()
    commitBuilder.setTreeId( treeId )
    commitBuilder.setMessage( s"Dummy commit to enhance history.\nreplaces file ${file.path}")
    commitBuilder.setParentId(parent)
    val person = new PersonIdent( "me", "me@example.com" )
    commitBuilder.setAuthor( person )
    commitBuilder.setCommitter( person )

    val objectInserter2 = repo.newObjectInserter();
    val commitId = objectInserter2.insert( commitBuilder );
    objectInserter2.flush();

    commitId

  }

  def Blame_File(commit:ObjectId, path:String) =
  {
    val blame = git.blame()
    blame.setStartCommit(commit)
    blame.setFilePath(path)
    blame.setFollowFileRenames(true)
    blame.setTextComparator(RawTextComparator.WS_IGNORE_ALL)
    val blameResult = blame.call()
    val blameText = blameResult.getResultContents()

    val size = blameText.size


    val listBlame = (0 to size-1).map { i =>
      val commit = blameResult.getSourceCommit(i)
      val line = blameResult.getSourceLine(i)
      (commit, line)
    }
    listBlame
  }

  def first_commit(path:String) = {
    val log = git.log.addPath(path).call()
    val logIt = log.asScala.toIterator.toList
    logIt.last
  }




  // we will assume that the input to this command is the
  // fileid, <commit> <fileid>, <commit> <fileid> etc

/*  val commit = args(0)
  val file = args(1)
  val anscommit = args(2)
  val ansfile = args(3)
 */

//in linux repo
  val commit = "8fe28cb58bcb235034b64cbbb7550a8a43fd88be"
  val file = "net/netfilter/nf_conntrack_h323_main.c"
  val ansCommit = "f587de0e2feb9eb9b94f98d0a7b7437e4d6617b4"
  val ansFile = "net/ipv4/netfilter/ip_conntrack_helper_h323.c"

/*
 //in token repo
  val commit    = "b8bb0909d85d71da604c4cf901b88dcf383228c2"
  val file      = "net/netfilter/nf_conntrack_h323_main.c"
  val ansCommit = "e5481b2d9f514bae33f6e556652ead331e804763"
  val ansFile   = "net/ipv4/netfilter/ip_conntrack_helper_h323.c"
 */

  println(file, ansCommit, ansFile)

  //blame original file
  val commitId = repo.resolve(commit);
  val ansCommitId = repo.resolve(ansCommit);

  def contents_at_commit(cid: ObjectId, path: String) = {
    val contentsBlobId = blob_at_commit(cid, path)
    val loader = repo.open(contentsBlobId)
    scala.io.Source.fromInputStream(loader.openStream).mkString
  }

  val contents = contents_at_commit(commitId, file).split("\n")

//  println(contents)

  val originalBlame = Blame_File(commitId, file)

  val firstCommit = first_commit(file)// determine which is the very first commit of the file

  val firstVersionBlob = blob_at_commit(firstCommit, file)
  assert(firstVersionBlob != null)

  val ansFileBlob = blob_at_commit(ansCommitId, ansFile)
  assert(ansFileBlob != null)

  // if the blob of last version of file is different than the blob ansfile
  val commitToBlame =
    if (firstVersionBlob != ansFileBlob) {
      // create dummy commit with ancestor commit
      val revCommit = get_rev_commit(ansCommitId)
      assert(revCommit != null)
      val fileToReplace = new File_Entry(ansFile, FileMode.REGULAR_FILE, firstVersionBlob)
      create_commit(repo, fileToReplace, revCommit)
    } else {
      ansCommitId
    }
  val secondBlame =Blame_File(commitToBlame, ansFile)

  // blame ansFile at commitToBlame

  // combine the blames
  originalBlame.zipWithIndex.foreach{case ((commit, lineOld),line)=>

    // if the commit is the same as the ancestor then we must
    // replace the blame info
    if (commit.getName == ansCommit) {

      val otherCommit = secondBlame(lineOld)._1


      if (otherCommit.getName == commitToBlame.getName) {
        print(commit.getName)
        print(" (")
        print ("--->done during copy<--")
        print(" " + commit.getAuthorIdent)
      } else {
        print(otherCommit.getName)
        print(" (")
        print ("--->original file<--")
        //print(otherCommit.getName)
        print(" " + otherCommit.getAuthorIdent)
      }
    } else {
      print(commit.getName)
      print(" (")
      print(" " + commit.getAuthorIdent)
    }
    println(s") ${contents(line)}");
  }

  System.exit(0);

/*
  println("traversing rest...")

  val head = commits.last
  println(head)
  commits_contents2(repo, head)

  println("end...")
 */

/*
  commits.take(1).foreach { c =>
    println("------------------")
    commits_contents(repo, c).foreach(println(_))
  }
 */
  // insert the new blob
  //val lastCommitId = new RevCommit(repo.resolve(Constants.HEAD));
/*


  val lastCommitId = commits.last

  val objectInserter = repo.newObjectInserter();
  val bytes = "Hello World! 232\n".getBytes( "utf-8" );
  val blobId = objectInserter.insert( Constants.OBJ_BLOB, bytes );
  objectInserter.flush();

  println("Blobid inserted " + blobId)

  val fileToReplace = new File_Entry( "xdiff/xdiff.h", FileMode.REGULAR_FILE, blobId )
  create_commit(repo, fileToReplace, lastCommitId)
 */

/*
  //we need to convert this
  println("contents of " + first)

  var i = 0
  while (treeWalk.next()) {

    println("get tree count" + treeWalk.getTreeCount())

    if (treeWalk.isSubtree()) {
        println("dir: " + treeWalk.getPathString());
        treeWalk.enterSubtree();
    } else {
        println("file: " + treeWalk.getPathString());
    }
    i = i + 1
  }
  println("size "+i)
 */

/*

  val git = Git.open(repoFile)

  val log = git.log.addPath(path).call()

  val logIt = log.asScala.toIterator.toList

  val startCommit = logIt.head.getName()


  def Blame_File(commit:String, path:String) =
  {
    val blame = git.blame()
    val commitObj = git.getRepository().resolve(commit);
    blame.setStartCommit(commitObj)
    blame.setFilePath(path)
    blame.setFollowFileRenames(true)

    val blameResult = blame.call()
    val blameText = blameResult.getResultContents()

    val size = blameText.size


    val listBlame = (0 to size-1).map { i =>
      val commit = blameResult.getSourceCommit(i)
      val line = blameResult.getSourceLine(i)
      (commit, line)
    }
    listBlame
  }


  val originalBlame = Blame_File(firstCommit, path)
  val secondBlame =Blame_File(dummyCommit, beforePath)


  originalBlame.zipWithIndex.foreach{case ((commit, lineOld),line)=>
    print(s"line [$line]")
    print(commit.getName)

    if (commit.getName == lastCommit) {
      val otherCommit = secondBlame(lineOld)._1

      if (otherCommit.getName == dummyCommit) {
        print ("--->done during the copy<--")
        print(" " + commit.getAuthorIdent)
      } else {
        print ("--->part of the original file<--")
        print(otherCommit.getName)
        print(" " + otherCommit.getAuthorIdent)
      }
    } else {
      print(" " + commit.getAuthorIdent)
    }

    println("");

  }

 */


  println("Hello, World! 2 ")

/*
  val invertBlame = secondBlame.map{case (line, commit, lineOld)=>
    (commit, lineOld)

  }
 */
  println("Hello, World! 3 ")





}
