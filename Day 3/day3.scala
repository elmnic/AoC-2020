/**
 * https://adventofcode.com/2020/day/3
 */

import scala.io.Source

object day3 {
	class Slope(startX: Int, startY: Int) {
		var x = startX
		var y = startY
		var trees = 0L // Part 2 answer requires Int > 32 bits
	}

	def main(args: Array[String]): Unit = {
		val source = Source.fromFile("input.txt")

		var slopes = scala.collection.mutable.Map[(Int,Int), Slope](
			(1, 1) -> new Slope(1, 1),
			(3, 1) -> new Slope(3, 1),
			(5, 1) -> new Slope(5, 1),
			(7, 1) -> new Slope(7, 1),
			(1, 2) -> new Slope(1, 2)
			)

		var part1 = 0L
		var part2 = 0L
		
		var x = 0
		var y = 0

		/** 
		 * Traverse (with duplication on the x-axis) and locate 'trees'
		 * Use the defined slope gradients to look at specific positions in the current line
		 * and count the number of trees for each slope
		 */

		 // Traverse along Y-axis
		for (line <- source.getLines()) {
			
			// Traverse along X-axis
			for (char <- line) {

				// Examine each slope
				for ((gradient, slope) <- slopes) {

					// Current character is on the path of the given slope
					if (x == slope.x && y == slope.y) {
						
						if (char == '#')
							slope.trees += 1

						// Loop the slope path's x-axis using modulo
						slope.x = (slope.x + gradient._1) % line.size
						slope.y += gradient._2
					}
				}
				x += 1
			}

			// Right end reached, loop back X but continue down Y
			x = 0
			y += 1
		}
		source.close()

		// Get part1 and part2 tree counts from Map of slopes
		part1 = slopes((3,1)).trees
		part2 = slopes.map(_._2.trees).product

		println("Part 1: " + part1)
		println("Part 2: " + part2)
	}
}