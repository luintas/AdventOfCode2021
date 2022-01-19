import scala.io.Source

object Crabs extends App {
  def parse(it: Array[String], map: Map[Int, Long]): Map[Int, Long] = {
    if (it.length == 0) {
      // println("A")
      return map
    } else {
      val key: Int = it(0).toInt
      val currentval: Long = map.getOrElse(key, 0L) + 1;
      val tmp = map + (key -> currentval);
      parse(it.slice(1, it.size), tmp)
    }
  }
  def ScoreAtpos(array: Array[(Int, Long)], pos: Int): Long = {
    if (array.isEmpty) {
      0L
    } else {
      ScoreAtpos(array.slice(1, array.length), pos) + (Range(0,(array(0)._1 - pos).abs+1).sum * array(0)._2)
      //Below is the answer for the first star
      // ScoreAtpos(array.slice(1, array.length), pos) + ((array(0)._1 - pos).abs * array(0)._2)
    }
  }
  def bestScore(
      array: Array[(Int, Long)],
      previousBest: Long,
      current: Int
  ): Long = {
    if (current == array.size) {
      return previousBest
    } else {
      val myresult = ScoreAtpos(array, current)
      if (myresult > previousBest) bestScore(array, previousBest, current + 1)
      else bestScore(array, myresult, current + 1)
    }
  }
  val lines = Source
    .fromFile(
    "/home/nell/Documents/AdventOfCode2021/day7/InputFile.txt"
    // "/home/nell/Documents/AdventOfCode2021/day7/TestFile.txt"
    )
    .getLines
    .next
    .split(",");
  var position: Map[Int, Long] =
    parse(lines, Map[Int, Long]()); //Position (X,Y)
  val array = position.toArray
  println(array.mkString(" "))
  println("Best Score is " + bestScore(array, Int.MaxValue, 0))
  // println("The submarine is now "+position._1+" unit from it's lateral position and "+position._2+" unit from it's vertical position");
  // println("Result "+(position._1 * position._2));
}
