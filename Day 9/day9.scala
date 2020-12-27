/**
 * https://adventofcode.com/2020/day/9
 */

import scala.io.Source
import scala.util.matching.Regex

object day9 {

	def main(args: Array[String]): Unit = {
		val source = Source.fromFile("input.txt")
		val preamble = 25
		var part1 = 0L
		var part2 = 0L

		val input = (for (line <- source.getLines()) yield line.toLong).toList

		/**
		  * Part 1, Return the number for which the preamble doesn't have two numbers summing to
		  *
		  * Sliding window of size 'preamble + 1'
		  * (for generating lists containing the actual preamble plus the number of interest)
		  *
		  * For each combination of two numbers in the preamble, check if the sum is equal to
		  * the number of interest
		  */
		part1 = input.sliding(preamble+1, 1)
			.filterNot(x => x.take(preamble).combinations(2).exists(
						(y => y.sum == x.reverse.head))
		).next().drop(preamble).head

		/**
		  * Part 2, Return the sum of the min and max numbers in the segment that sums up to Part1
		  *
		  * Iterate through all continuous segments with >= 2 values
		  * E.g. 1,2,3,4,5
		  *      2,3,4,5
		  *      3,4,5
		  *      4,5
		  *
		  * Create a stream that scans left-to-right and sums the values
		  * Take the numbers up to the index of the last number that caused the total sum to be == Part1
		  * E.g. (7) = [3,4,5].take(indexOf(2)) 3+4=7
		  *
		  * Return the sum of the min and max in the collection
		  */
		for ((num, idx) <- input.zipWithIndex) {

			val segment = input.takeRight(input.length - idx)
			val sums = segment.toStream.scanLeft(0L)(_ + _) // Lazy summing left-to-right
			val nums = segment.take(sums.lastIndexWhere(_ == part1)) // Take the segment up until where the sum is equal 'part1'

			if (nums.length >= 2)
				part2 = nums.min + nums.max

		}
		println("Part 1: " + part1)
		println("Part 2: " + part2)
		source.close()
	}
}