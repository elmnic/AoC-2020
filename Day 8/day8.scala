/**
 * https://adventofcode.com/2020/day/8
 */

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.immutable.Nil

object day8 {

	def main(args: Array[String]): Unit = {
		val source = Source.fromFile("input.txt")

		var part1 = 0
		var part2 = Int.MinValue

		val input = (for (line <- source.getLines()) yield line.split(" ").toList).toList

		var opSet = scala.collection.mutable.Set[(List[String], Int)]()

		/**
		 * Common return function for execution.
		 *
		 * Clears the loop-detecting Set for the next execution
		 *
		 * Returns the result
		 */
		def ret(result: Int): Int = {
			opSet.clear()
			result
		}

		def executePart1(bootCode: List[List[String]], index: Int = 0, acc: Int = 0): Int = bootCode(index) match {
			// Checks if the current code has already been run
			case op :: arg :: Nil if (opSet.add((bootCode(index), index))) => op match {
				case "nop" => executePart1(bootCode, index + 1, acc)
				case "acc" => executePart1(bootCode, index + 1, acc + arg.toInt)
				case "jmp" => executePart1(bootCode, index + arg.toInt, acc)
			}

			// Return result and clear Set of operations for
			case _ => ret(acc)
		}

		def executePart2(bootCode: List[List[String]], index: Int = 0, acc: Int = 0): (Boolean, Int) = bootCode(index) match {

			//
			case op :: arg :: Nil if (opSet.add((bootCode(index), index))) => op match {

				// Index is below the bottom instruction, terminate successfully
				case "nop" if (index + 1 >= bootCode.size) 			=> (true, ret(acc))
				case "acc" if (index + 1 >= bootCode.size)			=> (true, ret(acc + arg.toInt))
				case "jmp" if (index + arg.toInt >= bootCode.size) 	=> (true, ret(acc))

				// Normal execution
				case "nop" => executePart2(bootCode, index + 1, 		acc)
				case "acc" => executePart2(bootCode, index + 1, 		acc + arg.toInt)
				case "jmp" => executePart2(bootCode, index + arg.toInt, acc)
			}

			// opSet.add failed, loop detected
			case _ => (false, ret(acc))
		}

		// Retrieve the result
		part1 = executePart1(input)
		println("Part 1: " + part1)

		for ((code, index) <- input.zipWithIndex.filterNot(_._1.contains("acc"))) {
			/**
			 * Slice the boot code into pre- and post-sections depending on the current code and its Index
			 * Build it up like [Pre, current, Post] and then run executePart2 on each iteration
			 */
			val preCode = input.slice(0, index).filterNot(_.isEmpty)
			val postCode = input.slice(index + 1, input.size).filterNot(_.isEmpty)
			val curCode = {code match {
				case op :: arg :: Nil if (op == "jmp") => List("nop", arg)
				case op :: arg :: Nil if (op == "nop") => List("jmp", arg)
				case _ => code
			}}

			// Rebuild the boot code
			val uncorruptedCode = List(
				preCode.flatten,
				curCode,
				postCode.flatten
			).flatten.grouped(2).toList

			// Retrieve the result
			val result = executePart2(uncorruptedCode)
			if (result._1)
				part2 = result._2
		}

		println("Part 2: " + part2)
		source.close()
	}
}