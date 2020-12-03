package day3

import cats.implicits._
import cats.parse.Parser.{char, oneOf1}
import cats.parse.Parser1
import util.Ops._

import scala.io.Source
import scala.util.Using

object Day3 {

  trait Cell {
    def isTree(): Boolean
  }

  object Cell {
    def open: Cell = () => false
    def tree: Cell = () => true
  }

  case class Line(cells: Array[Cell])

  case class Grid(lines: Array[Line], columns: Int) {
    val rows: Int = lines.length
    def get(x: Int, y: Int): Cell = lines(y % rows).cells(x % columns)
  }

  def line: Parser1[Line] =
    oneOf1(List(char('.').as(Cell.open), char('#').as(Cell.tree))).rep1
      .map(x => Line(x.toList.toArray))

  def parseGrid(l: Seq[Line]): Either[String, Grid] =
    Either.cond(l.map(_.cells.length).distinct.size == 1, Grid(l.toArray, l.head.cells.length), "Not all lines have the same length")

  def main(args: Array[String]): Unit = {
    val grid = Using(Source.fromFile("src/main/scala/day3/input")) {
      _.getLines().filterNot(_.isEmpty).toList.traverse(line.parseAll)
        .left.map(_.toString)
        .flatMap(parseGrid)
    }.get.yolo

    def treesInPath(grid: Grid, slope: (Int, Int)): Long =
      LazyList.iterate((0, 0)) { case (x, y) => (x + slope._1, y + slope._2) }
        .takeWhile { case (_, y) => y < grid.rows }
        .count { case (x, y) => grid.get(x, y).isTree() }

    // part 1
    println(treesInPath(grid, (3, 1)))
    // part2
    println(
      List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
        .map(treesInPath(grid, _))
        .product
    )
  }
}
