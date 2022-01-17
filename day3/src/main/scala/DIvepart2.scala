import scala.io.Source
import scala.collection.mutable.Queue


//On a un tableau de tuple, chaque tuple représente un élement du mot ar[1] premierre lettre si 0 on incremente le tuple de gauche si 1 on incrémmente la seconde
object Divepart2 {
    def main(args: Array[String]) = {
        val lines = Source.fromFile("/home/nell/Documents/AdventOfCode2021/day3/InputFile.txt").getLines;
        var artextOxygen:Array[String] = Array.ofDim[String](0);
        var artextCO2:Array[String] = Array.ofDim[String](0);
        for(i <- lines ){
            artextOxygen=artextOxygen.appended(i);
            artextCO2=artextCO2.appended(i);
        }
        var ind = 0;
        while(artextOxygen.size >1){
            println(ind)
            val tmp = (parse(artextOxygen,mostCommonOxygen _))(ind)
            artextOxygen = artextOxygen.filter( (x)=>(x.charAt(ind).toInt-48 == tmp))
            ind+=1;
        }
        ind=0;
        while(artextCO2.size >1){
            println(ind)
            val tmp = (parse(artextCO2,mostCommonCO2 _))(ind)
            artextCO2 = artextCO2.filter( (x)=>(x.charAt(ind).toInt-48 == tmp))
            ind+=1;
        }
        println("Oxygen Generator Value is "+binaryTodecimal(artextOxygen(0))+" binary value = "+artextOxygen(0));
        println("Oxygen Generator Value is "+binaryTodecimal(artextCO2(0))+" binary value = "+artextCO2(0));
        println("Result "+(binaryTodecimal(artextOxygen(0)) * binaryTodecimal(artextCO2(0))));
    }
    def parse(str:String,array:Array[Array[Int]]):Array[Array[Int]] = {
        val cpt=0;
        for(i <- Range.apply(0,str.size)){
            array(str.charAt(i).toInt - 48)(i)+=1;
        }
        return array
    }
    def parse(arstr:Array[String],mostCommon:(Array[Array[Int]])=>(Array[Int])):Array[Int] = {
        val cpt=0;
        var array : Array[Array[Int]] = Array.ofDim[Int](2,arstr(0).size); //Position (X,Y)
        for(str:String <- arstr){
            for(i <- Range.apply(0,str.size)){
                array(str.charAt(i).toInt - 48)(i)+=1;
            }
        }
        return mostCommon(array)
    }
    def mostCommonOxygen(array:Array[Array[Int]]):Array[Int] = {
        val retVal = new Queue[Int];
        for(i <- Range.apply(0,array(0).length)){
            if(array(0)(i) > array(1)(i)) (retVal+=0)else(retVal+=1)
        }
        retVal.toArray
    }
    def mostCommonCO2(array:Array[Array[Int]]):Array[Int] = {
        val retVal = new Queue[Int];
        for(i <- Range.apply(0,array(0).length)){
            if(array(0)(i) > array(1)(i)) (retVal+=1)else(retVal+=0)
        }
        retVal.toArray
    }
    def gammaToEpsilon(array:Array[Int]):Array[Int] = {
        val retVal = new Queue[Int];
        for(i<- array){
            retVal+= (i +1)%2
        }
        retVal.toArray
    }
    def arrayToString(array:Array[Int]):String = {
        val sb = new StringBuilder
        for( i <-array){
            sb.append(i.toString())
        }
        sb.toString()
    }
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
    def binaryTodecimal(number: String): Long = {
		// Assuming that number contains 0,1s
		// Used to store result
		var result: Long = 0;
		var bit: Int = 0;
		var n: Int = number.length() - 1;
		// Execute given number in reverse order
		while (n >= 0)
		{
			if (number.charAt(n) == '1')
			{
				// When get binary 1
				result += (1 << (bit));
			}
			n = n - 1;
			// Count number of bits
			bit += 1;
		}
		// Display decimal result
		return result;
	}
}
