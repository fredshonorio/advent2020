package day1

import scala.io.Source
import scala.util.Using

object Part1And2 {
  def main(args: Array[String]): Unit = {
    val entries = Using(Source.fromFile("src/main/scala/day1/input")) {
      _.getLines().map(_.toInt).toList
    } .get

    def findNumbers(n: Int): Option[Int] =
      entries.combinations(n)
        .find(tuple => tuple.sum == 2020)
        .map(tuple => tuple.product)

    findNumbers(2).foreach(println) // part 1
    findNumbers(3).foreach(println) // part 2
  }

}
