import scala.io.Source

object Main {
    def main(args: Array[String]): Unit = {
        val lines = Source.fromFile("Input.txt").getLines.toList
        println(s"Lines count: ${lines.size}")

        val snailFishNumbers = lines.map(parse)

        println

        println("Before")
        for (sfn <- snailFishNumbers)
            println(sfn.pretty())

        println
        
        val result = snailFishNumbers.reduceLeft(addReduce)
        
        println

        println("After")
        println(result.pretty())

        println

        println(s"Magnitude ${magnitude(result)}")
    }

    def addReduce(left: SnailFishNumber, right: SnailFishNumber): SnailFishNumber = {
        val sum = add(left, right)
        reduce(sum)
    }

    def parse(line: String): SnailFishNumber = {
        parseChars(line.toCharArray())._1
    }

    def parseChars(chars: Array[Char]): (SnailFishNumber, Array[Char]) = {
        if (chars.head >= '0' && chars.head <= '9')
            (N(chars.head.toString.toInt), chars.tail)
        else { // [<left>,<right>]
            val (left, remaining1) = parseChars(chars.tail)
            val (right, remaining2) = parseChars(remaining1.tail)

            (P(left, right), remaining2.tail)
        }
    }

    def add(left: SnailFishNumber, right: SnailFishNumber): SnailFishNumber = {
        P(left, right)
    }

    def reduce(num: SnailFishNumber): SnailFishNumber = {
        var curr = num
        var reduced = reduceOnce(curr)

        while (curr != reduced) {
            curr = reduced
            reduced = reduceOnce(curr)
        }

        reduced
    }

    def reduceOnce(num: SnailFishNumber): SnailFishNumber = { 
        var (sfn, exploded) = explode(num, 0, false)
    
        if (exploded) 
            addExploded(sfn)
        else 
            split(sfn, false)._1
    }
    
    def explode(num: SnailFishNumber, pairDepth: Int, exploded: Boolean): (SnailFishNumber, Boolean) = {
        num match {
            case P(P(N(vLeft), N(vRight)), pRight) if (!exploded) => 
                if (pairDepth >= 3) {
                    (P(E(vLeft, vRight), pRight), true)
                } else {
                    val (explodeRight, explodedRight) = explode(pRight, pairDepth + 1, exploded)
                    (P(P(N(vLeft), N(vRight)), explodeRight), explodedRight)
                }
            case P(pLeft, P(N(vLeft), N(vRight))) if (!exploded) => 
                if (pairDepth >= 3) {
                    (P(pLeft, E(vLeft, vRight)), true)
                } else {
                    val (explodeLeft, explodedLeft) = explode(pLeft, pairDepth + 1, exploded)
                    (P(explodeLeft, P(N(vLeft), N(vRight))), explodedLeft)
                }
            case P(left, right) => {
                    val (explodeLeft, explodedLeft) = explode(left, pairDepth + 1, exploded)
                    val (explodeRight, explodedRight) = explode(right, pairDepth + 1, explodedLeft)
                    (P(explodeLeft, explodeRight), explodedRight)
                }
            case N(v) => (N(v), exploded)
        }
    }

    def addExploded(num: SnailFishNumber): SnailFishNumber = {
        val (addedLeftNum, _) = addExplodedLeft(num, 0)
        val (addedRightNum, _) = addExplodedRight(addedLeftNum, 0)
        addedRightNum
    }

    def addExplodedLeft(num: SnailFishNumber, add: Int): (SnailFishNumber, Int) = {
        num match {
            case E(addLeft, addRight) => (E(addLeft, addRight), addLeft)
            case N(v) => (N(v + add), 0)
            case P(left, right) => {
                val (addedRight, addRight) = addExplodedLeft(right, add)
                val (addedLeft, addLeft) = addExplodedLeft(left, addRight)

                (P(addedLeft, addedRight), addLeft)
            }
        }
    }

    def addExplodedRight(num: SnailFishNumber, add: Int): (SnailFishNumber, Int) = {
        num match {
            case E(addLeft, addRight) => (N(0), addRight) // Remove exploded number
            case N(v) => (N(v + add), 0)
            case P(left, right) => {
                val (addedLeft, addLeft) = addExplodedRight(left, add)
                val (addedRight, addRight) = addExplodedRight(right, addLeft)
                
                (P(addedLeft, addedRight), addRight)
            }
        }
    }

    def split(num: SnailFishNumber, splitted: Boolean): (SnailFishNumber, Boolean) = {
        num match {
            case N(v) if v > 9 && !splitted => {
                val half = v.toDouble / 2

                (P(N(math.floor(half).toInt), N(math.ceil(half).toInt)), true)
            }
            case N(v) => (N(v), splitted)
            case P(left, right) => {
                val (splitLeft, splittedLeft) = split(left, splitted)
                val (splitRight, splittedRight) = split(right, splittedLeft)
                (P(splitLeft, splitRight), splittedRight) 
            }
        }
    }

    def magnitude(num: SnailFishNumber): Long = {
        num match {
            case N(v) => v.toLong
            case P(left, right) => 3L * magnitude(left) + 2L * magnitude(right)
        }
    }
}

abstract class SnailFishNumber { 
    def pretty(): String 
}

case class P(val left: SnailFishNumber, val right: SnailFishNumber) extends SnailFishNumber {
    def pretty(): String = s"[${left.pretty()}, ${right.pretty()}]"    
}
case class N(val num: Int) extends SnailFishNumber {
    def pretty(): String = num.toString
}
case class E(val addLeft: Int, val addRight: Int) extends SnailFishNumber {
    def pretty(): String = "E"
}