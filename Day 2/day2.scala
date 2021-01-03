/**
 * https://adventofcode.com/2020/day/2
 */

import scala.io.Source

object day2 {
	def main(args: Array[String]): Unit = {
		val source = Source.fromFile("input.txt")

		var part1 = 0
		var part2 = 0

		for (line <- source.getLines()) {
			// Process input
			val input = line.split(" ")
			val range = input(0).split("-")
			val low = range(0).toInt
			val high = range(1).toInt
			val letter = input(1).take(1)
			val pwd = input(2)

			/**
			  * Part 1, count occurrences of the given character in the string
			  */
			var count = pwd.filter(_.toString == letter).size

			if (count <= high && count >= low)
				part1 += 1

			/**
			  * Part 2, Exclusive OR on the given character at the given positions
			  */
			val pos1 = pwd.charAt(low-1).toString()
			val pos2 = pwd.charAt(high-1).toString()

			if ((pos1 == letter) ^ (pos2 == letter))
				part2 += 1
		}

		println("Part 1: " + part1)
		println("Part 2: " + part2)
	}
}