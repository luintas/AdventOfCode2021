import scala.io.Source

object Dive {
    def parse(it:Iterator[String],tuple:(Int,Int)):(Int,Int) = {
        if (it.isEmpty){
            tuple;
        }
        else{
            val sentence = it.next.split(" ")
            sentence(0) match {
                case "forward" => ( parse(it,(tuple._1 + sentence(1).toInt,tuple._2) ) );
                case "down" => ( parse(it,(tuple._1,tuple._2+ sentence(1).toInt) ) );
                case "up" => ( parse(it,(tuple._1,tuple._2- sentence(1).toInt) ) );
            }
        }
         
    }
  def main(args: Array[String]) = {
        val lines = Source.fromFile("/home/nell/Documents/AdventOfCode2021/day2/InputFile.txt").getLines;
        var position : (Int,Int) = parse(lines,(0,0))   ; //Position (X,Y)
        println("The submarine is now "+position._1+" unit from it's lateral position and "+position._2+" unit from it's vertical position");
        println("Result "+(position._1 * position._2));
    }
}
