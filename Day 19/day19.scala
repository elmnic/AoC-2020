/**
 * https://adventofcode.com/2020/day/19
 */

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.immutable.Nil
import scala.collection.mutable.ListBuffer


object day19 {
	def main(args: Array[String]): Unit = {
		val source = Source.fromFile(args(0))
		var part1 = 0L
		var part2 = 0L

		val input = (for (line <- source.getLines()) yield line.filter(_ != '"')).toList

		val (rules, text) = input.splitAt(input.lastIndexWhere(_.contains(":")) + 1)

		// rules.foreach(println)
		// println(" ")
		// text.foreach(println)

		val inputRules = (for (rule <- rules) yield {
			val Array(k, v) = rule.split(": ")
			(k.toInt -> s" $v ")
		}).toMap

		// println(s"Input rules:")
		// inputRules.foreach(println)


		// Propagate a single rule
		def propagateRule(ruleNr: Int, ruleToPropagate: String, rules: Map[Int, String]) = {
			// println(s"Rule to propagate: $ruleNr -> $ruleToPropagate")
			// println("Old rules:")
			// rules.foreach(println)
			val newRules = rules.map{ case (key, value) => 
				(key, value.replaceAll(s" ?$ruleNr ?", s" ($ruleToPropagate) "))
			}
			// println("New rules:")
			// newRules.foreach(println)
			newRules
		}

		def loop(ruleNr: Int, ruleToPropagate: String, rules: Map[Int, String]): Map[Int, String] = ruleNr match {
			case 0 => propagateRule(ruleNr, ruleToPropagate, rules)
			case nr => 
				// println(s"Rule nr: $nr, rules: $rules")
				val updatedRules = propagateRule(nr, ruleToPropagate, rules)
				val nextRuleNr = nr - 1
				val nextRule = updatedRules(nextRuleNr)
				loop(nextRuleNr, nextRule, updatedRules)
		}

		val nrOfRules = inputRules.keys.max

		val output = loop(nrOfRules, inputRules(nrOfRules), inputRules)
		val regex = output(0).replaceAll(" ", "")
		part1 = text.count(_.matches(regex))


// ################## Part 2 #############

		val inputRulesPart2 = (inputRules.map { 
			case (key, value) if key == 8 => (key, " 42 | 42 8 ")
			case (key, value) if key == 11 => (key, " 42 31 | 42 11 31 ")
			case default => default
		}).toSeq.sortBy(_._1).map(_._2)
		
		// def propagateAllRules(rules: Map[Int, String]): Map[Int, String] = {
		// 	rules.foreach { case (key, value) => rules = propagateRule(key, value, rules)}
		// }

		// inputRulesPart2.map { case (key, value) => 
		// 	(key, value.replaceAll(s" ?$ruleNr ?", s" ($ruleToPropagate) "))
		// }

		def propagateRule(rule: (Int, String), rules: Seq((Int, String))): Seq((Int, String)) = match rule {
			case (key, value) => rules.map(_.replaceAll(s" ?$key ?", s" ($value) "))
		}

		var matchesPart2 = 0

		println(inputRulesPart2)


		propagateRule(inputRulesPart2)

		// println("Calculating matches...")
		// for (i <- 1 to 10) {
		// 	newOutput = propagateAllRules(inputRulesPart2)
		// 	val newRegex2 = newOutput(0).replaceAll(" ", "")
		// 	matchesPart2 = text.count(_.matches(newRegex2))
		// 	println(s"Loop #$i, matches: $matchesPart2")
		// }
		part2 = matchesPart2

		println("Part 1: " + part1)
		println("Part 2: " + part2)
	}
}
