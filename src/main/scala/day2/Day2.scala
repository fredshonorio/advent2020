package day2

import cats.implicits._
import cats.parse.Parser.{anyChar, char}
import cats.parse.{Numbers, Parser, Parser1}

import scala.io.Source
import scala.util.{Try, Using}

object Day2 {

  case class Policy(a: Int, b: Int, character: Char)
  case class Line(pol: Policy, pw: String)

  def line: Parser1[Line] = {
    def attempt[A](a: Try[A]): Parser[A] = a.toEither
      .left.map(_.getMessage)
      .fold(Parser.failWith, Parser.pure)

    val ws = Parser.charIn(" \t\r\n").rep.void
    val number = Numbers.nonNegativeIntString.flatMap(a => attempt(Try(a.toInt)))
    val word = Parser.charIn(('a' to 'z') ++ ('A' to 'Z')).rep1
      .map(chs => new String(chs.toList.toArray))

    val policy = (number ~ char('-') ~ number ~ ws ~ anyChar)
        .map { case ((((a, _), b), _), ch) => Policy(a, b, ch) }

    // example: 1-3 a: abcde
    (policy ~ char(':') ~ ws ~ word)
      .map { case (((pol, _), _), pw) => Line(pol, pw) }
  }

  implicit class StringOps(x: String) {
    def safeGet(n: Int): Option[Char] = if (n >= 0 && n < x.length) Some(x(n)) else None
  }

  def main(args: Array[String]): Unit = {

    val entries = Using(Source.fromFile("src/main/scala/day2/input")) {
      _.getLines().filterNot(_.isEmpty).toList.traverse(line.parseAll)
    }.get.getOrElse(List.empty)

    def pwIsValidOccurrence(l: Line): Boolean = {
      val Policy(min, max, character) = l.pol
      val count = l.pw.count(_ == character)
      count >= min && count <= max
    }

    def pwIsValidPosition(l: Line): Boolean = {
      val Policy(a, b, character) = l.pol
      val pw = l.pw
      pw.safeGet(a - 1).contains(character) != pw.safeGet(b - 1).contains(character)
    }

    println(entries.count(pwIsValidOccurrence))
    println(entries.count(pwIsValidPosition))
  }

}
