/**
 * https://adventofcode.com/2020/day/6
 */

import scala.io.Source

object day6 {
	def main(args: Array[String]): Unit = {
		val source = Source.fromFile("input.txt")

		var part1 = 0
		var part2 = 0
		var container = List[List[Char]]()
		for (line <- source.getLines()) {
			
			if (!line.isEmpty()) 
				container = container :+ line.toList
			else {

				// Part 1, Count number of unique questions answered
				part1 += container.reduce(_ union _).distinct.size
				
				// Part 2, Count number of questions answered by everyone
				part2 += container.reduce(_ intersect _).distinct.size
				
				container = List()
			}
			
		}
		source.close()

		println("Part 1: " + part1)
		println("Part 2: " + part2)
	}
}