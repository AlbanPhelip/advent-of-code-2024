package fr.sncf.itnovem.adventofcode2024

object Day05 extends AdventOfCode {

  private val rules: Array[(Int, Int)] = input
    .filter(_.contains("|"))
    .map(_.split("\\|").toList)
    .map(x => (x.head.toInt, x(1).toInt))

  case class Page(page: Int) extends Ordered[Page] {
    override def compare(that: Page): Int = {
      if (rules.contains((this.page, that.page))) 1
      else if (rules.contains((that.page, this.page))) -1
      else 0
    }
  }

  private def arePagesCorrectlySorted(pages: Array[Int]): Boolean = {
    pages
      .combinations(2)
      .map(x => (x(0), x(1)))
      .map(combination => !rules.exists(_.swap == combination))
      .reduce(_ && _)
  }

  override def fileName: String = "aoc05.txt"

  override def execute(): (Long, Long) = {

    val pageNumbers: Array[Array[Int]] = input
      .filter(_.contains(","))
      .map(_.split(",").map(_.toInt))

    val star1 = pageNumbers
      .filter(arePagesCorrectlySorted)
      .map(pages => pages(pages.length / 2))
      .sum

    val star2 = pageNumbers
      .filter(!arePagesCorrectlySorted(_))
      .map(_.map(Page).sorted)
      .map(pages => pages(pages.length / 2).page)
      .sum

    (star1, star2)
  }



}
