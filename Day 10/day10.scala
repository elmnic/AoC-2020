/**
 * https://adventofcode.com/2020/day/10
 */

import scala.io.Source
import scala.util.matching.Regex

object day10 {

	def main(args: Array[String]): Unit = {
		val source = Source.fromFile("input.txt")
		var part1 = 0
		var part2: BigInt = 0L
		
		val temp = List(0) ::: (for (line <- source.getLines()) yield line.toInt).toList
		val input = (temp ::: List(temp.max + 3)).sorted

		/**
		  * Part 1, Sort the list and find all groups of 2 that have a difference of 1 or 3
		  */
		val res1 = input.sliding(2).filter(x => x(1) - x(0) == 1)
		val res3 = input.sliding(2).filter(x => x(1) - x(0) == 3)
		part1 = res1.size * res3.size


		/**
		  * Part 2, Generate Map of all adapters and their possible next alternatives
		  * 
		  * Start from key 0 and explore all possible alternatives,
		  * store the result (number of valid paths) in another Map, 
		  * i.e dynamic programming.
		  * 
		  */
		val alts = 
		(for (i <- input) yield 
			(i -> (for
				(j <- input.filter(x => x != i && x-i <=3 && x-i > 0)) yield
					j)
			)
		).toMap
		
		var keysTested = scala.collection.mutable.Map[(Int, Int), BigInt]()

		part2 = helper(0)
		def helper(key: Int): BigInt = {
			if (key == alts.keys.max) 
				BigInt(1)
			else {
				// Counter for how many
				var valid: BigInt = 0
				for (alt <- alts(key)) {
					// Memoized result available 
					// Store the number of valid paths from this for the [key,alt] combination
					if (!keysTested.contains((key, alt)))
						keysTested += ((key, alt) -> helper(alt))
					
					valid += keysTested((key, alt))
				}
				valid
			}
		}

		println("Part 1: " + part1)
		println("Part 2: " + part2)
		
	}
}
