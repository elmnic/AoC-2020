/**
 * https://adventofcode.com/2020/day/15
 */

import scala.io.Source
import scala.util.matching.Regex

object day15 {
	def main(args: Array[String]): Unit = {
		val source = Source.fromFile(args(0))
		var part1 = 0
		var part2 = 0

		val input = (for (line <- source.getLines()) yield line.split(",").toList.map(_.toInt)).next()

		// 								[NumberSpoken, (Previous, 2nd Previous)]
		// Initialize ageMap with the input numbers
		var ageMap = scala.collection.mutable.Map[Int, (Int, Int)]()
		var previousNumber = 0
		for ((num, idx) <- input.zipWithIndex) {
			ageMap.put(num, (idx+1, -1))
			previousNumber = num
		}
		var turn = input.length
		while (turn <= 30000000) {

			// Default number to speak
			var numberToSpeak = 0

			// If previous number has been spoken before
			if (ageMap.contains(previousNumber)) {

				// If it is not the first time
				if (ageMap(previousNumber)._2 != -1) {

					// Take the diff between the two most recent turns of the previous number
					numberToSpeak = ageMap(previousNumber)._1 - ageMap(previousNumber)._2
				}
			}

			// Has the number to speak been spoken before?
			if (ageMap.contains(numberToSpeak)) {

				// Move the most recent turns one step
				ageMap.put(numberToSpeak, (turn, ageMap(numberToSpeak)._1))
			}
			// Add the number for the first time
			else {
				ageMap.put(numberToSpeak, (turn, -1))
			}
			previousNumber = numberToSpeak

			if (turn == 2020)
				part1 = previousNumber
			if (turn == 30000000)
				part2 = previousNumber

			turn += 1

		}

		println("Part 1: " + part1)
		println("Part 2: " + part2)
	}
}
