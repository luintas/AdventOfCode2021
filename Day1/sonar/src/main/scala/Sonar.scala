import scala.io.Source


object  Sonar {
    def parse(it:Iterator[String],previousDepth:Int,cpt:Int):Int ={
        if(it.isEmpty)(cpt);
        else{
            val currentDepth = it.next().toInt
            if(currentDepth.toInt > previousDepth){
                parse(it,currentDepth,cpt+1);
            }
            else
                parse(it,currentDepth,cpt);
        }
    }
    def main(args: Array[String]) = {
        val lines = Source.fromFile("/home/nell/Documents/AdventOfCode2021/Day1/sonar/InputFile.txt").getLines
        var previousDepth:Int = lines.next().toInt;
        var cptIncrease : Int = parse(lines,previousDepth,0); //Count of the number of time the depth increased
        println("The depth detected by the Sonar increased "+cptIncrease+" times");
    }
}
