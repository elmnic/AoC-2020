/**
 * https://adventofcode.com/2020/day/7
 */

import scala.io.Source
import scala.util.matching.Regex

object day7 {

	def main(args: Array[String]): Unit = {
		val source = Source.fromFile("input.txt")

		var part1 = 0
		var part2 = 0

		var contentMap = scala.collection.mutable.Map[String, List[String]]()

		// Get the bags and what they contain
		for (line <- source.getLines()) {
			var Array(contain, content) = (line.split("contain"))
			contain = "^.*(?= bags)".r.findAllMatchIn(contain).mkString
			val bags = "((?<= )(.*)(?= bags?)|no other)".r.findAllIn(content.replace(',', '\n')).toList
			contentMap += (contain -> bags)
		}
		source.close()
	
		/**
		  * Part 1, Find the number of bags that eventually will contain a Shiny Gold bag
		  * Recursively traverse depth-first until either a 'Shiny Gold' or 'No Other' is found
		  * 
		  * The map only contains the key color of the contained bags
		  */
		var part1Map = contentMap.clone()
								.mapValues(_.map("((?<=\\d )(.*)|no other)".r
								.findAllIn(_).mkString))
		
		// Depth-first-search checking if the current bag eventually leads to a "shiny gold" bag
		def part1Check(content: List[String]): Int = content match {
			case Nil => 0
			case head :: tl if (head == "shiny gold") => 1
			case head :: tl if (head == "no other") => part1Check(tl)
			case head :: tl if (part1Check(part1Map(head)) == 0) => part1Check(tl)
			case _ => 1
		}
		
		for ((container, content) <- part1Map) {
			if (container != "shiny gold")
				part1 += part1Check(content)
		}
		
		/**
		  * Part 2, Find the number of bags contained in one Shiny Gold bag
		  * Recursively build up the answer starting from the contents of a Shiny Gold bag
		  * 
		  * The map contains both the number of each bag, as well as the key color of the bag itself
		  */
		var part2Map = contentMap.clone()
		def part2Check(content: List[String]): Int = content match {
			case Nil => 0
			case head :: tl if (head == "no other") => 0
			case head :: tl => {
				val num = "\\d".r.findAllIn(head).mkString.toInt // Get the number
				val key = "(?<=\\d )(.*)".r.findAllIn(head).mkString // Get the next key
				num + num * part2Check(part2Map(key)) + part2Check(tl)
			}
		}

		part2 = part2Check(part2Map("shiny gold"))

		println("Part 1: " + part1)
		println("Part 2: " + part2)
	}
}