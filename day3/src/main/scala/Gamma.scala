import scala.io.Source
import scala.collection.mutable.Queue


object Gamma {
    def main(args: Array[String]) = {
        val lines = Source.fromFile("/home/nell/Documents/AdventOfCode2021/day3/TestFile.txt").getLines;
        val firstword = lines.next()
        var accumulator : Array[Array[Int]] = Array.ofDim[Int](2,firstword.size); 
        accumulator = parse(accumulator,lines)
        val gamma = mostCommon(accumulator);
        val epsilon = gammaToEpsilon(gamma);
        println("Gamma value is "+gamma.mkString);
        println("Epsilon value is "+epsilon.mkString);
        println("Result is "+(binaryTodecimal(gamma) +" "+ binaryTodecimal(epsilon)) +" "+ (binaryTodecimal(gamma) * binaryTodecimal(epsilon)));
    }
    def parse(array:Array[Array[Int]],it:Iterator[String]):Array[Array[Int]] = {
        if(it.isEmpty){
            array
        }
        else{
            def tmp(array:Array[Array[Int]],str:String,i:Int):Array[Array[Int]] = {
                if (i == array(0).length)(array)
                else{
                    array(str.charAt(i).toInt - 48)(i)+=1;
                    tmp(array,str,i+1);
                }
            }
            return parse(tmp(array,it.next(),0),it)
        }
    }
    def mostCommon(array:Array[Array[Int]]):Array[Int] = {
        def tmp(array:Array[Array[Int]],accumulator:Queue[Int]):Array[Int] = {
            if(accumulator.size == array(0).length)(accumulator.toArray)
            else{
                val i = accumulator.size;
                if(array(0)(i) > array(1)(i)) 
                    (tmp(array,accumulator+=0))  
                else
                    (tmp(array,accumulator+=1))
            }
        }
        tmp(array,new Queue[Int])
    }
    def gammaToEpsilon(array:Array[Int]):Array[Int] = {
        def tmp(array:Array[Int],queue:Queue[Int]):Array[Int] = {
            if(queue.size == array.length)(queue.toArray)
            else{
                tmp(array,queue+=((array(queue.size))+1)%2)
            }
        }
        tmp(array,new Queue[Int])
    }
    //I didn't write the function below
    def binaryTodecimal(number: Array[Int]): Long = {
		// Assuming that number contains 0,1s
		// Used to store result
		var result: Long = 0;
		var bit: Int = 0;
		var n: Int = number.size - 1;
		// Execute given number in reverse order
		while (n >= 0)
		{
			if (number(n) == 1)
			{
				// When get binary 1
				result += (1 << (bit));
			}
			n = n - 1;
			// Count number of bits
			bit += 1;
		}
		// Display decimal result
        result
	}
}
