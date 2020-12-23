/**
 * https://adventofcode.com/2020/day/13
 */

import scala.io.Source
import scala.util.matching.Regex

object day13 {
	def main(args: Array[String]): Unit = {
		val source = Source.fromFile("input.txt")
		var part1 = 0
		var part2 = 0
		
		val input = (for (line <- source.getLines()) yield line.split(",").filterNot(_ == "x").toList.map(_.toInt)).toList

		part1 = {
			val earliestTime = input(0)(0)
			val busID = input(1).minBy(x => x - earliestTime % x)
			val busEarliest = (busID * ((earliestTime / busID) + 1))
			(busEarliest - earliestTime) * busID
			
		}

		println("Part 1: " + part1)
		println("Part 2: " + part2)
		
	}
}
