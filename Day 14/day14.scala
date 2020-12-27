/**
 * https://adventofcode.com/2020/day/14
 */

import scala.io.Source
import scala.util.matching.Regex

object day14 {
	def main(args: Array[String]): Unit = {
		val source = Source.fromFile(args(0))
		var part1 = BigInt(0)
		var part2 = BigInt(0)

		val input = (for (line <- source.getLines()) yield line.split(" = ").toList)
			.toList
			.map(
				x => x match {
					case x if x(0).contains("mem") => List("mem", "\\d+".r.findAllIn(x(0)).next(), x(1))
					case x => x
			})

		var memory1 = scala.collection.mutable.Map[Int, BigInt]()
		var memory2 = scala.collection.mutable.Map[BigInt, BigInt]()

			def maskValue(mask: List[Char], value: String, isAddressMask: Boolean = false): String = {
				val maskedValue = (
					// Perform masking operation on the value
					for (i <- 0 until 36) yield {
						mask(i) match {

							// Part 2 stuff
							case x if (isAddressMask && x == 'X') => 'X'
							case x if (isAddressMask && x == '0') => value(i)
							case x if (isAddressMask && x == '1') => '1'

							// Part 1 stuff
							case x if (x != 'X') => x
							case _ 				 => value(i)
					}
				}).mkString
				return maskedValue
			}

			def maskAddress(mask: List[Char], value: String, acc: List[Char] = List()): Unit = mask match {
				case Nil => {
					// val maskedAdr = maskValue(acc, mem, true)
					// printf("mem: %s\nacc: %s\nadr: %s\nval: %s\n\n", mem, acc.mkString, maskedAdr, value)
					memory2.put(BigInt(acc.mkString, 2), BigInt(value))
				}

				// "Floating" bit, expand into each direction, i.e. 0 and 1
				case h :: t if (h == 'X') => {
					maskAddress(t, value, acc :+ '0')
					maskAddress(t, value, acc :+ '1')
				}
				case h :: t => maskAddress(t, value, acc :+ h)
			}

		/**
		  * Part 1, Mask each value assignment and sum the final values at each address
		  */
		var mask = ""
		for (op <- input) {
			op.head match {
				case "mask" => mask = op.tail.head
				case "mem" => op.tail match {
					case mem :: value :: Nil => {
						// Part 1, Store masked value at address 'mem'
						// Pad the input value's binary representation to 36 bits
						val paddedValue = value.toInt.toBinaryString.reverse.padTo(36, '0').reverse
						val maskedValue = maskValue(mask.toList, paddedValue)
						memory1.put(mem.toInt, BigInt(maskedValue, 2))

						// TODO: Side effects! It is doing a recursive "tree" expansion that
						// TODO: represents each permutated bitmask
						// Part 2, Store 'value' at masked address
						val paddedAdr = mem.toInt.toBinaryString.reverse.padTo(36, '0').reverse
						maskAddress(maskValue(mask.toList, paddedAdr, true).toList, value)
					}
				}
			}
		}
		part1 = memory1.values.sum
		part2 = memory2.values.sum

		println("Part 1: " + part1)
		println("Part 2: " + part2)

	}

}
