import scala.io.Source

object Main {
    def main(args: Array[String]): Unit = {
        val lines = Source.fromFile("Input.txt").getLines.toList
        println(s"Lines count: ${lines.size}")

        val snailFishNumbers = lines.map(parse)

        println

        /*
        for (sfn <- snailFishNumbers)
            println(sfn)
        */

        println(reduce(snailFishNumbers.head))


    }

    def parse(line: String): SnailFishNumber = {
        parseChars(line.toCharArray())._1
    }

    def parseChars(chars: Array[Char]): (SnailFishNumber, Array[Char]) = {
        if (chars.head >= '0' && chars.head <= '9')
            (N(chars.head.toInt), chars.tail)
        else { // [<left>,<right>]
            val (left, remaining1) = parseChars(chars.tail)
            val (right, remaining2) = parseChars(remaining1.tail)

            (P(left, right), remaining2.tail)
        }
    }

    def reduce(num: SnailFishNumber): SnailFishNumber = {
        reduce(num, 0)
    }

    def reduce(num: SnailFishNumber, pairDepth: Int): SnailFishNumber = {
        num match {
            case P(P(N(nLeft), N(nRight)), pRight) => if (pairDepth >= 3) {
                                                        
                                                      }
            case P(left, right) => P(reduce(left, pairDepth + 1), reduce(right, pairDepth + 1))
            case 
        }
    }
}


abstract class SnailFishNumber {}

case class P(val left: SnailFishNumber, val right: SnailFishNumber) extends SnailFishNumber {}
case class N(val num: Int) extends SnailFishNumber {}



