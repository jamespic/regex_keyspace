import scala.util.parsing.combinator._

sealed trait Regex
final case class Chr(c: Char) extends Regex {
    override def toString = c.toString
}
final case class ChrRange(from: Chr, to: Chr) extends Regex {
    override def toString = from.toString + "-" + to.toString
}
final case class ChrSet(chrs: List[Regex]) extends Regex {
    override def toString = chrs.mkString("[","","]")
}
sealed case class SplitGroup(options: List[Regex]) extends Regex {
    override def toString = options.mkString("(","|",")")
}
final class Opt(x: Regex) extends SplitGroup(List(x, TermList(Nil))) {
    override def toString = x + "?"
}
final case class TermList(terms: List[Regex]) extends Regex {
    override def toString = terms.mkString
}

class RegexAST extends RegexParsers {
    def chr = "[A-Z0-9 ]".r ^^
                    {s => Chr(s.charAt(0))}
    def chrRange = chr ~ "-" ~ chr ^^
                    {case c1 ~ "-" ~ c2 => ChrRange(c1, c2)}
    def chrSet = "[" ~ rep(chrRange | chr) ~ "]" ^^
                    {case "[" ~ chrs ~ "]" => ChrSet(chrs)}
    def singleChr = chr | chrSet
    def group: Parser[SplitGroup] = "(" ~ repsep(termList, "|") ~ ")" ^^
                    {case "(" ~ chrs ~ ")" => SplitGroup(chrs)}
    def option = (singleChr | group) ~ "?" ^^
                    {case x ~ "?" => new Opt(x)}
    def term = option | group | singleChr
    def termList = rep(term) ^^ (TermList(_))
    override def skipWhitespace = false
}


object RegexAST extends RegexAST {
    private val rand = new java.util.Random
    private val doubleMax = BigDecimal(Double.MaxValue).toBigInt
    def parse(s: String): ParseResult[TermList] = parse(termList, s)
    def testParse(s: String) = {
        val res = parse(s).get
        assert(res.toString == s)
        res
    }
    def combinations(r: Regex): BigInt = r match {
        case Chr(_) => BigInt(1)
        case ChrRange(Chr(x), Chr(y)) => y - x + BigInt(1)
        case ChrSet(chrs) => (chrs map combinations).sum
        case SplitGroup(options) => (options map combinations).sum
        case TermList(terms) => (terms map combinations).product
    }
    
    def random(r: Regex): String = r match {
        case Chr(x) => x.toString
        case ChrRange(Chr(x), Chr(y)) => (rand.nextInt(y - x) + x).toChar.toString
        case ChrSet(chrs) => random(selectWeighted(chrs))
        case SplitGroup(options) => random(selectWeighted(options))
        case TermList(terms) => (terms map random).mkString
        case x => selectOne(allMatches(x))
    }
    
    def allMatches(r: Regex): Stream[String] = r match {
        case Chr(x) => Stream(x.toString)
        case ChrRange(Chr(x), Chr(y)) => Stream.range(x, (y + 1).toChar) map (_.toString)
        case ChrSet(chrs) => chrs.toStream flatMap allMatches
        case SplitGroup(options) => options.toStream flatMap allMatches
        case TermList(Nil) => Stream("")
        case TermList(head :: tail) =>
                for (h <- allMatches(head);
                     t <- allMatches(TermList(tail))) yield h + t
    }
    
    private def selectOne[T](s: Stream[T]): T = selectOne(s.toList)
    private def selectOne[T](s: List[T]): T = {
        val size = s.size
        val i = rand.nextInt(size)
        s(i)
    }
    private def selectWeighted(s: List[Regex]): Regex = {
        val weights = s map {x => (x, combinations(x))}
        val totalWeight = (weights map (_._2)).sum
        if (totalWeight > doubleMax) {
            selectOne(s)
        } else {
            val total = totalWeight.toDouble
            val r = rand.nextDouble * total
            var pos = 0.0
            for ((x, w) <- weights) {
                pos += w.toDouble
                if (pos >= r) return x
            }
            weights.last._1
        }
    }
    
    def main(args: Array[String]) {
        val regex = parse(args(0)).get
        args.tail match {
            case Array() | Array("count") => println(combinations(regex))
            case Array("all") =>
              for (s <- allMatches(regex)) println(s)
            case Array(n) =>
              for (i <- 0 until n.toInt) println(random(regex))
        }
    }
}
