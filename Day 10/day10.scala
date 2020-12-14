/**
 * https://adventofcode.com/2020/day/10
 */

import scala.io.Source
import scala.util.matching.Regex

object day10 {

	def main(args: Array[String]): Unit = {
		val source = Source.fromFile("input.txt")
		var part1 = 0L
		var part2 = 0L
		
		val initialInput = List(0L) ::: (for (line <- source.getLines()) yield line.toLong).toList
		val input = initialInput ::: List(initialInput.max + 3L)

		/**
		  * Part 1, Sort the list and find all groups of 2 that have a difference of 1 or 3
		  */
		val res1 = input.sorted.sliding(2).filter(x => x(1) - x(0) == 1L)
		val res3 = input.sorted.sliding(2).filter(x => x(1) - x(0) == 3L)
		part1 = res1.size * res3.size


		println("Part 1: " + part1)
		println("Part 2: " + part2)
		source.close()
	}
}