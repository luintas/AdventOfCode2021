import scala.io.Source
import scala.collection.immutable.Queue

object Lanternfish extends App {
  def parse(it: Array[Int], count: Array[Long]): Array[Long] = {
    if (it.length == 0) {
      // println("A")
      count
    } else {
      count(it(0)) += 1;
      parse(it.slice(1, it.length), count)
    }
  }
  //If you ever read this in the future you should find a way to make this be Functionnal rether than using a mutable Array
  def resultAfterXDays(today: Array[Long], currentDay: Int, X: Int): Long = {
    if (currentDay > X) {
      today.sum
    } else {
      today((currentDay + 7) % 9) += today(currentDay % 9)
      resultAfterXDays(today, currentDay + 1, X)
    }
  }
  val lines = Source
    .fromFile(
      // "/home/nell/Documents/AdventOfCode2021/day6/InputFile.txt"
      "/home/nell/Documents/AdventOfCode2021/day6/TestFile.txt"
    )
    .getLines()
  val listStartDay = lines.next().split(",").map(_.toInt)
  val firstDay = parse(listStartDay, Array.ofDim(9))
  println(firstDay.mkString(" "))
  println(firstDay.sum)
  println(resultAfterXDays(firstDay, 0, 255))

}
