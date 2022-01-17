import scala.io.Source


object  Bingov2 {
    val selected = -255;
    def getArray(it:Iterator[String]):Array[Array[Int]]= {
        val str = it.next()
        // println(str)
        if(str.isBlank)(new Array[Array[Int]](0))
        else
            Array(str.split(" ").filter(!_.isEmpty()).map(_.toInt)) ++ getArray(it);//use a parse instead of a split
    }
    def getAllArrays(it:Iterator[String]):Option[List[Array[Array[Int]]]] = {
        if(it.isEmpty) (Some(List()))
        else
            Some( List(getArray(it)) ++ getAllArrays(it).getOrElse(List()) )
    }
    def markNumber(bingo:Array[Array[Int]],number:Int):Array[Array[Int]] = {
        bingo.map(_.map((x)=>(if(x == number)(selected)else(x))))//Lookup how to create static value in Scala and set selected value to that
    }
    def sum(bingo:Array[Array[Int]]):Int = {
        bingo.map(_.map((x)=>(if(x == selected)(0)else(x)))).map(_.sum).sum
    }
    //Check if a line have been completed
    def checkLine(bingo:Array[Array[Int]]):Boolean = {
        return ( bingo.count((x)=>(x.count(_== selected) == x.length)) > 0) 
    }
    def won(bingo:Array[Array[Int]]):Boolean = {
        return checkLine(bingo) || checkLine(bingo.transpose)
    }
    def idk(lst : List[Array[Array[Int]]],it : Array[Int]):Int = {
        val newlst = lst.map(markNumber(_,it(0))).map(x=>(x,won(x))).filter( ! _._2).map(_._1) // remove the grid who won
        // println(newlst.size)
        // println(newlst.last.map(_.mkString(" ") ).mkString("\n"))
        // println("\n"+it(0)+"\n")
        println(lst.length)
        if ( newlst.length > 1) {
            idk(newlst,it.slice(1,it.size))
        }
        aux(newlst,it.slice(1,it.size))
    }
    def aux(lst : List[Array[Array[Int]]],it : Array[Int]):Int = {
        val newlst = lst.map(markNumber(_,it(0)))
        println(won(newlst.last),it(0))
        println(newlst.last.map(_.mkString(" ") ).mkString("\n"))
        val someoneWon:Int = newlst.map(x=>(x,won(x))).filter(_._2).map(x=>(sum(x._1))).lastOption.getOrElse(0)
        if(someoneWon != 0){
            someoneWon * it(0)
        }
        else
            aux(newlst,it.slice(1,it.size))
    }
    def main(args: Array[String]) = {
        val lines = Source.fromFile("/home/nell/Documents/AdventOfCode2021/day4/InputFile.txt").getLines
        val first:Array[Int]= lines.next.split(",").map(_.toInt);
        val pickedNumbers = first.clone()

        //  println(first.iterator.mkString(" "))
        // lines.next()
        val tmp:List[Array[Array[Int]]] =getAllArrays(lines).getOrElse(List())
        //  println(pickedNumbers.next())
        println(idk(tmp.filter(! _.isEmpty),pickedNumbers));
    }
}