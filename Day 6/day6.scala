/**
 * https://adventofcode.com/2020/day/6
 */

import scala.io.Source

object day6 {
	def main(args: Array[String]): Unit = {
		val source = Source.fromFile("input.txt")

		var part1 = 0
		var part2 = 0
		var questions1 = scala.collection.mutable.Set[Char]()
		var questions2 = scala.collection.mutable.Map[Char, Int]()
		var counter = 0
		for (line <- source.getLines()) {
			
			// There are answers
			if (!line.isEmpty()) {

				// Increment number of respondants
				counter += 1

				// Loop through the answers
				for (char <- line) {
				
					// Part 1, Add to set for non-duplicate checking
					questions1.add(char)

					// Part 2, Increment count of answer
					if (questions2.contains(char))
						questions2 += (char -> (questions2(char) + 1))
					else
						questions2 += (char -> 1)
				}
			}
			else {

				// Part 1, Add sum of unique answers 
				part1 += questions1.size
				questions1.clear()

				// Part 2, Add sum of answers that every person answered
				part2 += questions2.values.count(_ == counter)
				questions2.clear()

				counter = 0
			}
			
		}
		source.close()

		println("Part 1: " + part1)
		println("Part 2: " + part2)
	}
}