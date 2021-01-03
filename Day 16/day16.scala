/**
 * https://adventofcode.com/2020/day/16
 */

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer

object day16 {
	def main(args: Array[String]): Unit = {
		val source = Source.fromFile(args(0))
		var part1 = 0
		var part2 = 0L

		val input = (for (line <- source.getLines()) yield line).toList.filter(!_.isBlank())

		// Split the input
		var (inRules, rest) = input.splitAt(input.indexWhere(_.contains("your ticket")))
		var (inYour, inNearby) = rest.splitAt(rest.indexWhere(_.contains("nearby ticket")))
		inYour = inYour.drop(1) // Drop the text "your ticket"
		inNearby = inNearby.drop(1) // Drop the text "nearby tickets"

		// Map the ticket numbers to ints
		val yourTicket = inYour.map(_.split(",").toList.map(_.toInt)).flatten
		val nearbyTickets = inNearby.map(_.split(",").toList.map(_.toInt))

		// Extract the word and the intervals from the input
		val pattern = "([\\w ]+): ([\\d]+-[\\d]+) or ([\\d]+-[\\d]+)".r

		// Map the word to the valid numbers
		val rules = inRules.map(_ match {
			case pattern(what, num1, num2) => {
				val Array(a1, b1) = num1.split("-").map(_.toInt)
				val Array(a2, b2) = num2.split("-").map(_.toInt)
				(what -> ((a1 to b1).toSet ++ (a2 to b2).toSet))
			}
		}).toMap

		/**
		  * Part 1, Get the tickets that are not valid at all
		  */
		part1 = nearbyTickets.map(
			x => x.filterNot(
				y => rules.values.exists(_.contains(y))
			)
		).flatten.sum


		// Get all tickets that are completely valid
		val validTickets = nearbyTickets
			.filter(x => x.forall(y => rules.values.exists(_.contains(y))))

		// Get all potential categories for the categories
		var orderBuffer =
		(for (col <- (validTickets :+ yourTicket).transpose) yield {
			val potentialCategories = rules.filter(x => col.forall(y => x._2.contains(y)))
				.keySet
				.toList
			potentialCategories
		}).zipWithIndex

		// Get all potential columns for the categories
		val cats = (
			for (cat <- rules.keys) yield {
				var cols = orderBuffer.filter(_._1.contains(cat)).map(_._2)
				(cat, cols)
			}
		).toList


		// Visual aid showcasing how to reduce each increasingly larger list of cols
		for (rule <- cats.sortBy(_._2.length)) {
			println(rule._1 + "\n" + rule._2)
		}

		/**
		  * Part 2, find the correct column order
		  *
		  * Traverse in descending order (order._2.length) and diff against lists of length 1
		  * Remove the idx that is known to be the only alternative from all other categories
		  */
		def reduceCat(list: List[(String, List[Int])], acc: List[Int] = List()): List[(String, List[Int])] = list match {
			case Nil => Nil
			case (cat, idx) :: tl => {
				val keep = (idx diff acc).head
				(cat, List(keep)) :: reduceCat(tl, keep :: acc)
			}
		}

		val sortedCats = cats.sortBy(_._2.length)
		val correctOrder = reduceCat(sortedCats)

		val depCols = correctOrder.filter(_._1.contains("departure")).map(_._2).flatten
		part2 = yourTicket
			.zipWithIndex
			.filter{ case (value, idx) => depCols.contains(idx) }
			.map(_._1.toLong)
			.product

		println("Part 1: " + part1)
		println("Part 2: " + part2)
	}
}
