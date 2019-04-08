import scala.io.Source
import sys.process._


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
