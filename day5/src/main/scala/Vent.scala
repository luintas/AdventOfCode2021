import scala.io.Source
import scala.annotation.switch

object Main extends App {

  def createRange(val1: Int, val2: Int) = {
    val tmp = 0;
    if (val1 < val2) {
      Range.apply(val1, val2 + 1)
    } else {
      Range.apply(val1, val2 - 1, -1)
    }
  }
  //Use the start and end posititon of the vent to create a map of their positions
  def aux(start: (Int, Int), end: (Int, Int)): List[(Int, Int)] = {
    var min, max = (0, 0)
    if (start._1 + start._2 < end._1 + end._2) {
      min = start;
      max = end;

    } else {
      min = end;
      max = start;
    }
    (min) match {
      case _ if (min._1 == max._1) => {
        Range.apply(min._2, max._2 + 1).map((min._1, _)).toList
      }
      case _ if (min._2 == max._2) =>
        Range.apply(min._1, max._1 + 1).map((_, min._2)).toList
      case _ => {
        val res = createRange(min._1, max._1);
        val res2 = createRange(min._2, max._2);
        res.zip(res2).toList
      } 
    }
  }
  def parse(
      line: List[((Int, Int), (Int, Int))],
      resultLst: List[(Int, Int)]
  ): Int = {
    if (line.isEmpty) {

      resultLst
        .groupBy[(Int, Int)](x => x)
        .map(_._2.length)
        .filter(_ > 1)
        .size
    } else
      parse(
        line.slice(1, line.size),
        (resultLst.appendedAll(aux(line(0)._1, line(0)._2)))
      )
  }
  def formatInput(lst: List[String]): List[((Int, Int), (Int, Int))] = {
    lst
      .map(_.split(" -> "))
      .map(x =>
        (
          (
            (x(0).split(",")(0).toInt, x(0).split(",")(1).toInt),
            (x(1).split(",")(0).toInt, x(1).split(",")(1).toInt)
          )
        )
      )
  }
  val lines = Source
    .fromFile("/home/nell/Documents/AdventOfCode2021/day5/InputFile.txt")
    // .fromFile("/home/nell/Documents/AdventOfCode2021/day5/TestFile.txt")
    .getLines
  val formattedLines = formatInput(lines.toList);
  // println((keepLeft.map(_._1) ++ keepLeft.map(_._2)).max)
  // println((keepRight.map(_._1) ++ keepRight.map(_._2)).max)
  println("Result =" + parse(formattedLines, List[(Int, Int)]()))
}
