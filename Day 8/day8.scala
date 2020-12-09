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
		var part2 = 0
		
		var bootCode = (for (line <- source.getLines()) yield line.split(" ").toList).toSeq

		var opSet = scala.collection.mutable.Set[(List[String], Int)]()

		def ret(result: Int): Int = {
			opSet.clear()
			result
		}

		def executePart1(code: List[String], index: Int, acc: Int = 0): Int = code match {
			// Case runs if the code with the given index hasn't been run yet
			case op :: arg :: Nil if (opSet.add((code, index))) => op match {
				case "nop" => executePart1(bootCode(index + 1), 			index + 1, 			acc)
				case "acc" => executePart1(bootCode(index + 1), 			index + 1, 			acc + arg.toInt)
				case "jmp" => executePart1(bootCode(index + arg.toInt), 	index + arg.toInt, 	acc)
			}

			// Case for when a code already has been run
			case _ => ret(acc)
		}

		def executePart2(code: List[String], index: Int, acc: Int = 0): Int = code match {
			case op :: arg :: Nil if (opSet.add((code, index))) => index match {
				case _ if (index + 1 >= bootCode.size) => op match {
					case "nop" => println(code + " " + index); executePart2(Nil, index + 1, acc)
					case "acc" => println(code + " " + index); executePart2(Nil, index + 1, acc + arg.toInt)
					case "jmp" => println(code + " " + index); executePart2(Nil, index + arg.toInt, acc)
				}
				case _ => op match {
					case "nop" => println(code + " " + index); executePart2(bootCode(index + 1), index + 1, acc)
					case "acc" => println(code + " " + index); executePart2(bootCode(index + 1), index + 1, acc + arg.toInt)
					case "jmp" => println(code + " " + index); executePart2(bootCode(index + arg.toInt), index + arg.toInt, acc)
				}
			}

			// TODO: Differentiate loop and end-of-instruction termination
			case Nil => ret(acc)
			case _ => ret(acc)
		}
		
		part1 = executePart1(bootCode(0), 0)

		part2 = executePart2(bootCode(0), 0)
		
		println("Part 1: " + part1)
		println("Part 2: " + part2)

		source.close()
	}
}