import sys.process._
import scala.io.StdIn._
import java.io.File
//import util.Try
import scala.language.postfixOps

object  doGitDir {


  def usage(msg:String="") = {
    println(msg)
    println("Usage sourceDir outputDir outputExtension script " )
    println("  script is expected to output to stdout (it is a command to be passed to the shell)")
    System.exit(1)
  }

  def main(args: Array[String]) {

    if (args.length != 4) {
      usage()
    }

    val inputDir = args(0)
    val outputDir = args(1)
    val suffix = args(2)
    val command = args(3)

    if (command.length == 4) {
      usage("no command specified")
    }

    def process_file(x:String) {

      print(s"Processing [$x]: ")

      val inputFile = new File(s"$inputDir/$x")

      val outputFile = new File(s"$outputDir/$x"+suffix)

      if (outputFile.exists()) {

        println (s"output file exists [$outputFile]...");

        if (outputFile.length > 0) {
          println("   not processing\n");
          return
        } else {
          println("   but it is empty\n");
        }
      }

      val tempFile = File.createTempFile("prefix-", "-temp")
      tempFile.deleteOnExit();

      println(s"$inputDir/$x -> $outputDir/$x -> $tempFile")

      val outDir = new File(outputFile.getParent())

      if (!outDir.exists()) {
        outDir.mkdirs()
      }
      println(s"$outDir -> $outputFile\n")

      // run the script, returns error code
      val toRun = s"$command $inputFile"

      //println(s"To run [$toRun]")
      val result = (toRun #> tempFile) !

      if (result != 0) {
        println(s"error code $result executing")
        System.exit(1)
      }
      if (!tempFile.renameTo(outputFile)) {
        println ("unable to rename file");
        System.exit(1);
      }


    }

    val files = (s"git -C $inputDir ls-files " !!)

    val regexp = """\.[ch]$""".r
//    val regexp = """\.java$""".r

    val toProcess = files.split("\n").filter {s => regexp.findFirstIn(s).isDefined }.par

    println(toProcess.size)

    println("Number of threads", toProcess.tasksupport.parallelismLevel)

    toProcess.map(x=> process_file(x))

  }
}

