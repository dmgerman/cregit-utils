import sys.process._
import scala.io.StdIn._
import java.io.File
import util.Try


object  doGitDir {


  def main(args: Array[String]) {

    val inputDir = args(0)
    val outputDir = args(1)
    val suffix = args(2)
    val command = args(3)

    if (command.length == 0) {
      println("no command specified")
      System.exit(1)
    }

    def process_file(x:String) {

      println(s"Processing [$x]")

      val tempFile = File.createTempFile("prefix-", "-temp")
      tempFile.deleteOnExit();

      println(s"$inputDir/$x -> $outputDir/$x -> $tempFile")

      val inputFile = new File(s"$inputDir/$x")

      val outputFile = new File(s"$outputDir/$x"+suffix)

      val outDir = new File(outputFile.getParent())

      if (!outDir.exists()) {
        outDir.mkdirs()
      }
      println(s"$outDir\n\n")

      // run the script, returns error code
      val toRun = s"$command $inputFile"

      println(s"To run [$toRun]")
      val result = (toRun #> tempFile) !

      if (result != 0) {
        println(s"error code $result executing")
        System.exit(1)
      }

      tempFile.renameTo(outputFile)

    }


    val files = (s"git -C $inputDir ls-files " !!)

    val regexp = """\.[ch]$""".r

    val toProcess = files.split("\n").filter {s => regexp.findFirstIn(s).isDefined }.par

    println("Number of threads")
    println(toProcess.tasksupport.parallelismLevel)

    toProcess.map(x=> process_file(x))

  }
}

