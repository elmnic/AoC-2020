/**
 * https://adventofcode.com/2020/day/11
 */

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.immutable.Nil

object day11 {
	def main(args: Array[String]): Unit = {
		val source = Source.fromFile("input.txt")
		var part1 = 0
		var part2 = 0

		val input = (for (line <- source.getLines()) yield line.toList).toList

		/**
		 * Part 1, Create new board from the previous one
		 *
		 * Iterate through all seats and, for each seat, iterate through the surrounding seats.
		 * Store the number of occupied seats (#),
		 * match on the previous seat and return the new state for the seat.
		 */
		var stable = false
		var prev = input
		while (!stable) {
			val next =
			(for (row <- 0 until prev.length) yield
				(for (col <- 0 until prev(row).length) yield {

					// Find the surrounding occupied seats
					val seats = (for (x <- -1 to 1; y <- -1 to 1
									if !(x == 0 && y == 0);
									if row + y >= 0 && row + y < prev.length;
									if col + x >= 0 && col + x < prev(row).length
									) yield prev(row+y)(col+x)
								).filter(_ == '#').length

					// yield the new seat with respect to the rules
					prev(row)(col) match {
						case '#' if (seats >= 4) => 'L'
						case 'L' if (seats == 0) => '#'
						case _ => prev(row)(col)
					}
				}).toList
			).toList

			prev = next
		}

		/**
		  * Part 2, Same principle but iterate along each direction
		  */
		stable = false
		prev = input
		while (!stable) {
			val next =
			(for (row <- 0 until prev.length) yield
				(for (col <- 0 until prev(row).length) yield {


					/**
					  * TODO: Attempt to do this more functionally like in Part 1 using 'yield'
					  * TODO: remove !visibleSeats.contains and just yield the first instance of '#' or 'L',
					  * TODO: otherwise yield nothing. The resulting list of seats should still contain the correct amount of '#'
					  */
					// Find the surrounding visible seats
					var visibleSeats = scala.collection.mutable.Map[(Int, Int), Char]()

					// Skip if there is no seat
					if (prev(row)(col) != '.') {

						// Loop through all directions
						for (x <- -1 to 1; y <- -1 to 1) {

							// Exclude the current seat
							if (!(x == 0 && y == 0)) {

								// Expand the direction until outside the board or an occurance is found
								var rad = 1
								while (row + y*rad >= 0 && row + y*rad < prev.length &&
									col + x*rad >= 0 && col + x*rad < prev(row).length &&
									!visibleSeats.contains((y, x))) {

									// Find first occurance of Occupied or Empty, add to Map
									if (prev(row + y*rad)(col + x*rad) == '#' || prev(row + y*rad)(col + x*rad) == 'L')
										visibleSeats.put((y, x), prev(row + y*rad)(col + x*rad))

									rad += 1
								}

								// Place default value in dir-map
								if (!visibleSeats.contains((y, x)))
									visibleSeats.put((y, x), '.')
							}
						}
					}
					val seats = visibleSeats.values.toList.filter(_ == '#').length

					// yield the new seat with respect to the rules
					prev(row)(col) match {
						case '#' if (seats >= 5) => 'L'
						case 'L' if (seats == 0) => '#'
						case _ => prev(row)(col)
					}
				}).toList
			).toList

			// Equilibrium reached, break loop and return the occupied seats
			if (next == prev) {
				stable = true
				part2 = next.flatten.filter(_ == '#').length
			}
			prev = next
		}

		println("Part 1: " + part1)
		println("Part 2: " + part2)

	}
}
