import scala.io.Source 
import scala.collection.immutable.Queue

object  Sonarpart2 {
    def parse(it:Iterator[String],previousDepth:Int,currentQueue:Queue[Int],cpt:Int):Int ={
        if(it.isEmpty)(cpt);
        else{
            val (trash:Int,tmpQueue:Queue[Int]) = currentQueue.dequeue
            val newQueue:Queue[Int] = tmpQueue.enqueue(it.next.toInt);
            val currentDepth:Int = newQueue.sum;
            println(newQueue.size == currentQueue.size)
            if(currentDepth.toInt > previousDepth){
                parse(it,currentDepth,newQueue,cpt+1);
            }
            else
                parse(it,currentDepth,newQueue,cpt);
        }
    }
    def main(args: Array[String]) = {        
        val lines = Source.fromFile("/home/nell/Documents/AdventOfCode2021/Day1/sonar/InputFile.txt").getLines
        // var previousSum:Int = window.sum(Numeric[Int]); //current results from the Sonar
        val window: Queue[Int]= Queue(lines.next().toInt,lines.next().toInt,lines.next().toInt); // Represent the window of value to account for in the sum
        println(window.size)
        var cptInc : Int = parse(lines,window.sum,window,0); //Count of the number of time the depth increased
        println("The depth detected by the Sonar increased "+cptInc+" times");
    }
}
