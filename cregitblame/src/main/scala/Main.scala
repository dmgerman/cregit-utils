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

class File_Entry(val path: String, val fileMode: FileMode, val oid: ObjectId) {

}

class Blame_Entry(val commit: RevCommit, val line: Int, val filename: String) {

}

class Blame_File(val latestCommit: ObjectId, val fileName: String,
                 val firstCommit : ObjectId, val blameData: List[Blame_Entry],
                 val contents: Array[String])  {

  def get_line(line: Int) = {
    blameData(line)
  }

  def combine(other: Blame_File) : Blame_File = {
    // isDummyCommit indicates if joiningCommit was created to join the histories

    // make sure we have first commit
    val joiningCommit = other.latestCommit
    val isDummyCommit = joiningCommit.getName != firstCommit.getName

    assert(firstCommit!=null)
    assert(joiningCommit!= null)
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
    assert(contents.length ==  blameData.length)
    blameData.zip(contents).foreach{ case (entry, line) =>
      println(s"${entry.commit.getName} ${entry.filename} (${entry.commit.getAuthorIdent}) ${line}")
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
      println(s"we did not find file [$path] in commit " + commitid.getName)
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

  def do_blame_file(latestCommit:ObjectId, path:String, contents: Array[String]) :Blame_File = {
    val blame = git.blame()
    blame.setStartCommit(latestCommit)
    blame.setFilePath(path)
    blame.setFollowFileRenames(true)
    blame.setTextComparator(RawTextComparator.WS_IGNORE_ALL)
    val blameResult = blame.call()
    val blameText = blameResult.getResultContents()

    val size = blameText.size

    val listBlame = (0 to size-1).map { i =>
      val commit = blameResult.getSourceCommit(i)
      val line = blameResult.getSourceLine(i)
      val path = blameResult.getSourcePath(i);
      new Blame_Entry(commit, line, path)
    }

    new Blame_File(latestCommit, path, first_commit(path), listBlame.toList, contents)
  }

  //  def combine_blames()

  def first_commit(path:String) = {
    val log = git.log.addPath(path).call()
    val logIt = log.asScala.toIterator.toList
    logIt.last
  }

  def contents_at_commit(cid: ObjectId, path: String) = {
    val contentsBlobId = blob_at_commit(cid, path)
    val loader = repo.open(contentsBlobId)
    scala.io.Source.fromInputStream(loader.openStream).mkString
  }

}

object Main extends App {

//  val repoDir = "/home/linux/original.repo/linux-all-grafted/"
//  val repoDir = "/home/dmg/git.w/git-stein"
//  val repoDir = "/tmp/git"
//  val repoDir = "/tmp/token"

////////////////////////////////////////////////////////


/*  val commit = args(0)
  val file = args(1)
  val anscommit = args(2)
  val ansfile = args(3)
 */

//in linux repo
  val repoDir = "/tmp/linux"
  val commit = "8fe28cb58bcb235034b64cbbb7550a8a43fd88be"
  val file = "net/netfilter/nf_conntrack_h323_main.c"
  val ansCommit = "f587de0e2feb9eb9b94f98d0a7b7437e4d6617b4"
  val ansFile = "net/ipv4/netfilter/ip_conntrack_helper_h323.c"

/*
 //in token repo
  val repoDir = "/tmp/token"
  val commit    = "b8bb0909d85d71da604c4cf901b88dcf383228c2"
  val file      = "net/netfilter/nf_conntrack_h323_main.c"
  val ansCommit = "e5481b2d9f514bae33f6e556652ead331e804763"
  val ansFile   = "net/ipv4/netfilter/ip_conntrack_helper_h323.c"
 */
  val repo = new My_Repo(repoDir)

  val commitId = repo.resolve(commit);

  if (commitId == null) {
    Usage(s"latest commit for file Commit [$commit] does not exist in repo");
  }

  def Usage(st:String) {
    System.err.println("Usage...")
    System.err.println(st)
    System.exit(1)
  }


  val ansCommitId = repo.resolve(ansCommit);
  if (ansCommitId == null) {
    Usage(s"commit of version of ancestor of file [$ansCommit] does not exist in repo");
  }

  val contents = repo.contents_at_commit(commitId, file).split("\n")
  if (contents == null) {
    Usage(s"File ${file} does not exist in commit [$commit] in repo");
  }

  val originalBlame = repo.do_blame_file(commitId, file, contents)

  val firstVersionBlob = repo.blob_at_commit(originalBlame.firstCommit, file)
  assert(firstVersionBlob != null)

  val ansFileBlob = repo.blob_at_commit(ansCommitId, ansFile)
  assert(ansFileBlob != null)

  // if the blob of last version of file is different than the blob ansfile
  val commitToBlame =
    if (firstVersionBlob != ansFileBlob) {
      // create dummy commit with ancestor commit as parent
      // and the contents of the firstCommit of the file we are tracking
      val fileToReplace = new File_Entry(ansFile, FileMode.REGULAR_FILE, firstVersionBlob)
      repo.create_commit(fileToReplace, ansCommitId)
    } else {
      // othewise we just blame from the ancestor latest commit
      ansCommitId
    }

  //println(s">>>>>> joining commit ${commitToBlame.getName}")
  val secondBlame = repo.do_blame_file(commitToBlame, ansFile, Array[String]())

  val newBlame = originalBlame.combine(secondBlame)
  newBlame.output


}
