/**
 * https://adventofcode.com/2020/day/1
 */

import scala.io.Source

object day1 {
	def main(args: Array[String]): Unit = {
		val source = Source.fromFile("input.txt")
		var numbers = scala.collection.mutable.Set[Int]()

		var part1 = 0
		var part2 = 0

		for (line <- source.getLines()) {
			val num = line.toInt
			val diff1 = 2020 - num
			if (numbers.contains(diff1))
				part1 = diff1 * num
			for (i <- numbers) {
				val diff2 = diff1 - i
				if (numbers.contains(diff2)) {
					part2 = i * num * diff2
				}
			}
			numbers.add(num)
		}
		println("Part 1: " + part1)
		println("Part 2: " + part2)
	}
}