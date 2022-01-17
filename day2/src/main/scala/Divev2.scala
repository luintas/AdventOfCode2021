import scala.io.Source

object Divev2 {
    def main(args: Array[String]) = {
        val lines = Source.fromFile("/home/nell/Documents/AdventOfCode2021/day2/InputTest.txt").getLines;
        // var positionAndAim : (Int,Int,Int) = (0,0,0); //Position (X,Y)
        // var positionAndAim : (Int,Int,Int) = (0,0,0); //Position (X,Y)
        var positionAndAim : (Int,Int,Int) = parse(lines,(0,0,0)); //Position (X,Y)

        //for(i <- lines ){
        //    positionAndAim = parse(i,positionAndAim);
        //}
        println("The submarine is now "+positionAndAim._1+" unit from it's lateral position and "+positionAndAim._2+" unit from it's vertical position");
        println("Result "+(positionAndAim._1 * positionAndAim._2));
    }

    //Iterate on the text and update the tuple value in accordance
    def parse(it:Iterator[String],tuple:(Int,Int,Int)):(Int,Int,Int) = {
        if (it.isEmpty){
            tuple;
        }
        else{
            val sentence = it.next.split(" ")
            sentence(0) match {
                case "forward" => ( parse(it,(tuple._1 + sentence(1).toInt, tuple._2 + ( tuple._3 *  sentence(1).toInt ) ,tuple._3) ));
                case "down" => ( parse(it,(tuple._1,tuple._2,tuple._3+ sentence(1).toInt) ));
                case "up" => ( parse(it,(tuple._1,tuple._2,tuple._3- sentence(1).toInt) ));
            }
        }
         
    }
}
