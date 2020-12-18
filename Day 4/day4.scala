/**
 * https://adventofcode.com/2020/day/4
 */

import scala.io.Source
import scala.util.matching.Regex

object day4 {
	def validateKeyVal(key: String, value: String): Boolean = key match {
		case "byr" => return value.length == 4 && value.toInt >= 1920 && value.toInt <= 2002
		case "iyr" => return value.length == 4 && value.toInt >= 2010 && value.toInt <= 2020
		case "eyr" => return value.length == 4 && value.toInt >= 2020 && value.toInt <= 2030
		case "hgt" => {
			if ("[0-9]+(cm|in)+".r.findAllIn(value).length > 0) {
				val split = value.splitAt(value.length() - 2)
				split._2 match {
					case "cm" => return split._1.toInt >= 150 && split._1.toInt <= 193
					case "in" => return split._1.toInt >= 59  && split._1.toInt <= 76
				}
			}
			return false
		}
		case "hcl" => return "#[0-9a-f]{6}".r.findAllIn(value).length > 0
		case "ecl" => return "(amb|blu|brn|gry|grn|hzl|oth){1}".r.findAllIn(value).length > 0
		case "pid" => return "^[0-9]{9}$".r.findAllIn(value).length > 0 // && value.length() == 9
		case "cid" => return true
	}

	def main(args: Array[String]): Unit = {
		val source = Source.fromFile("input.txt")

		var part1 = 0
		var part2 = 0

		var passportFieldsPart1 = scala.collection.mutable.Map[String, Boolean](
			"byr" -> false,
			"iyr" -> false,
			"eyr" -> false,
			"hgt" -> false,
			"hcl" -> false,
			"ecl" -> false,
			"pid" -> false,
			"cid" -> false
		)
		var passportFieldsPart2 = passportFieldsPart1.clone()

		for (line <- source.getLines()) {
			
			// Line contains fields
			if (!line.isEmpty()) {
				val fields = line.split(" ")
				
				for (field <- fields) {
					val split = field.split(":")
					val key   = split(0)
					val value = split(1)

					// Part 1, no validation
					passportFieldsPart1.put(key, true)
					
					// Pat 2, with validation
					if (validateKeyVal(key, value))
						passportFieldsPart2.put(key, true)
				}
			}
			
			// End of passport reached, check acquired fields
			else {

				// Treat "cid" as always valid
				passportFieldsPart1.put("cid", true)
				passportFieldsPart2.put("cid", true)
				
				// Increment counter if all values are true
				if (passportFieldsPart1.values.reduce(_ && _))
					part1 += 1
				if (passportFieldsPart2.values.reduce(_ && _))
					part2 += 1
					
				// Reset fields
				for ((k, v) <- passportFieldsPart1)
					passportFieldsPart1.put(k, false)
				for ((k, v) <- passportFieldsPart2)
					passportFieldsPart2.put(k, false)
			}
		}
		source.close()

		println("Part 1: " + part1)
		println("Part 2: " + part2)
	}
}