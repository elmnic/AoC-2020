/**
 * https://adventofcode.com/2020/day/5
 */

import scala.io.Source

object day5 {
	def main(args: Array[String]): Unit = {
		val source = Source.fromFile("input.txt")

		var part1 = 0
		var part2 = 0

		// Part 1, find the maximum ID
		var seatIDs = scala.collection.mutable.Map[String, Int]()
		for (line <- source.getLines()) {

			val colPart = line.take(7)
			val rowPart = line.takeRight(3)
			var rows = List.range(0, 128)
			var cols = List.range(0, 8)

			// Get the row
			for (char <- colPart) {
				char match {
					case 'F' => rows = rows.take(rows.length/2)
					case 'B' => rows = rows.takeRight(rows.length/2)
				}
			}

			// Get the col
			for (char <- rowPart) {
				char match {
					case 'L' => cols = cols.take(cols.length/2)
					case 'R' => cols = cols.takeRight(cols.length/2)
				}
			}
			seatIDs += (line -> (rows(0) * 8 + cols(0)))
		}
		source.close()

		part1 = seatIDs.values.max

		// Part 2, find the missing ID
		var ids = List.range(seatIDs.values.min, seatIDs.values.max+1)
		val seatSet = seatIDs.values.toSet
		part2 = ids.filter(!seatSet.contains(_))(0)

		println("Part 1: " + part1)
		println("Part 2: " + part2)
	}
}