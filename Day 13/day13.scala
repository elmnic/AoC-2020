/**
 * https://adventofcode.com/2020/day/13
 */

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.immutable.Nil

object day13 {
	def main(args: Array[String]): Unit = {
		val source = Source.fromFile(args(0))
		var part1 = 0
		var part2 = BigInt(0)

		val input = (for (line <- source.getLines())
						yield line.split(",")
								.toList.map(x => x match {
									case x if x == "x" 	=> -1
									case x 				=> x.toInt
					})).toList

		// Part 1 stuff
		val earliestTime = input.head.head
		val buses = input.takeRight(1).head

		part1 = runPart1(earliestTime, buses)
		part2 = runPart2(buses.zipWithIndex.filter(_._1 > -1))

		println("Part 1: " + part1)
		println("Part 2: " + part2)

	}

	/**
	  * Part 1, Calculate the product of the difference of times
	  *
	  * The bus with the closest time is retreieved by taking the minimum value by
	  * an equation. the bus' next earliest time is calculated by rounding the
	  * quotient of the earliest time and the bus ID, and then multiplying by the bus ID
	  *
	  */
	def runPart1(time: Int, buses: List[Int]): Int = {
			val busID = buses.filter(_ > -1).minBy(x => x - time % x)
			val busEarliestTime = (busID * (time / busID + 1))
			(busEarliestTime - time) * busID
		}

	/**
	  * Part 2, Returns the earliest time that the sequence is in sync
	  *
	  * Once a part of the sequence is found the 'increment's are increased
	  * by a factor of LCM(busID left, busID right).
	  *
	  * Otherwise, just increase the time using the 'increment'
	  *
	  */
	def runPart2(busInput: List[(Int, Int)]): BigInt = {

		def helper(buses: List[(Int, Int)], time: BigInt, incr: BigInt): BigInt = buses match {
			// End reached, return answer
			case Nil => time

			// If the condition is met, continue on tail and increment the 'incr' by LCM(incr,id), i.e. incr*id
			case (id, idx) :: tl if ((time + idx) % id == 0) => helper(tl, time, incr * id)

			// Condition not met, increment time and keep looking
			case _ => helper(buses, time + incr, incr)
		}

		helper(busInput.tail, busInput.head._1, busInput.head._1)
	}
}
