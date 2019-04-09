import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.lib.FileMode


class File_Entry(val path: String, val fileMode: FileMode, val oid: ObjectId) {

}

class Blame_Entry(val commit: RevCommit, val line: Int, val filename: String) {

}



class Blame_File(val latestCommit: ObjectId, val fileName: String,
                 val firstCommit : ObjectId, val blameData: Array[Blame_Entry],
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

    System.err.println(s"joining this first commit [$firstCommit] with other latest commit [$joiningCommit]\n")


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
  //      println(s"${i+1} ${entry.commit.getName} ${entry.filename} (${entry.commit.getAuthorIdent}) ${line}")
       println(entry.commit.getName + ";;\t" + line)
    }
  }

  def output_n(n:Int) = {
    // make sure we have as many lines in the contents as in the blame
    // otherwise something is terribly wrong
    assert(contents.length ==  blameData.length, s"Length of contents [${contents.length}] is different than blame data [${blameData.length}]")
    blameData.
      take(n).
      zip(
        contents.zipWithIndex).
      foreach{ case (entry, (line, i)) =>
        println(s"${i+1} ${entry.commit.getName} ${entry.filename} (${entry.commit.getAuthorIdent}) ${line}")
        //println(entry.commit.getName + ";;\t" + line)
      }
  }


}
