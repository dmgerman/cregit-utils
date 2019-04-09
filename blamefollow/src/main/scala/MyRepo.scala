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
    System.err.println("running blame: " + blameCommand)

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

    val blameLines = recordsBlame.map { lines0 =>
      val lines = lines0.filter(_ != "boundary")
      val commitLine = lines(0)

      val prevFileNameLine = if (lines(10).startsWith("previous")) lines(11) else lines(10)
//      assert(prevFileName.startsWith("filename"))
      val commitPattern = "^([0-9a-f]{40}) ([0-9]+) .+$".r
//      println(commitLine)
      val commitPattern(cid, prevLineNumber) = commitLine
      //      println("after")
//      println(s"--->[$prevFileNameLine]")
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
