/**
 * https://adventofcode.com/2020/day/12
 */

import scala.io.Source
import scala.util.matching.Regex

object day12 {

	class Vec() {
		var x = 0
		var y = 0
		def this(x: Int, y: Int) {
			this()
			this.x = x
			this.y = y
		}

		// TODO: Overload "+="
		def +(that: Vec) = new Vec(this.x + that.x, this.y + that.y)
		def -(that: Vec) = new Vec(this.x - that.x, this.y - that.y)
		def *(that: Int) = new Vec(this.x * that, this.y * that)
	}

	class Ship() {
		var pos = new Vec()
		var dir = new Vec()
		def this(dir: Vec) {
			this()
			this.dir = dir
		}

		/**
		  * Update the position using the current position and by
		  * constructing a new Vec with the "amount" in the corresponding axis
		  */

		def movePart1(action: String, amount: Int): Unit = action match {
			case "N" => this.pos = this.pos + new Vec(0, amount)
			case "S" => this.pos = this.pos + new Vec(0, -amount)
			case "E" => this.pos = this.pos + new Vec(amount, 0)
			case "W" => this.pos = this.pos + new Vec(-amount, 0)
			case "F" => {
				this.pos = this.pos + this.dir*amount
			}
		}

		// Easy 90-degree rotation using the pattern (x,y) => (y, -x)
		def rotatePart1(degrees: Int, dir: Vec): Unit = degrees match {
			case 0 => this.dir = dir
			case _ => rotatePart1(degrees - 1, new Vec(dir.y, -dir.x))
		}

		/**
		  * Update the position using the current position and by
		  * constructing a new Vec with the "amount" in the corresponding axis
		  */
		def movePart2(action: String, amount: Int): Unit = action match {
			case "N" => this.dir = this.dir + new Vec(0, amount)
			case "S" => this.dir = this.dir + new Vec(0, -amount)
			case "E" => this.dir = this.dir + new Vec(amount, 0)
			case "W" => this.dir = this.dir + new Vec(-amount, 0)
			case "F" => {
				this.pos = this.pos + this.dir*amount
			}
		}

		// Easy 90-degree rotation using the pattern (x,y) => (y, -x)
		def rotatePart2(degrees: Int, dir: Vec): Unit = degrees match {
			case 0 => this.dir = dir
			case _ => rotatePart2(degrees - 1, new Vec(dir.y, -dir.x))
		}
	}

	def main(args: Array[String]): Unit = {
		val source = Source.fromFile("input.txt")
		var part1 = 0
		var part2 = 0

		/**
		  * Read the input.
		  * Map each line to be a (String, Int).
		  * Convert every L-rotation to the equivalent R-rotation.
		  */
		val input = (for (line <- source.getLines()) yield line).toList.map(t => {
			val (dir, arg) = t.splitAt(1)
			dir match {
				case "N" => ("N", arg.toInt)
				case "S" => ("S", arg.toInt)
				case "E" => ("E", arg.toInt)
				case "W" => ("W", arg.toInt)
				case "L" => ("R", ((360 - arg.toInt) / 90))
				case "R" => ("R", arg.toInt / 90)
				case "F" => ("F", arg.toInt)
			}
		})

		// Run every action using the ship's methods
		def runPart1(input: List[(String, Int)]): Int = {
			val ship = new Ship(new Vec(1, 0))
			for ((action, amount) <- input) {
				action match {
					case "R" => ship.rotatePart1(amount, ship.dir)
					case _ => ship.movePart1(action, amount)
				}
			}
			Math.abs(ship.pos.x) + Math.abs(ship.pos.y)
		}

		// Run every action using the ship's methods
		def runPart2(input: List[(String, Int)]): Int = {
			val ship = new Ship(new Vec(10, 1))
			for ((action, amount) <- input) {
				action match {
					case "R" => ship.rotatePart2(amount, ship.dir)
					case _ => ship.movePart2(action, amount)
				}
			}
			Math.abs(ship.pos.x) + Math.abs(ship.pos.y)
		}

		part1 = runPart1(input)
		part2 = runPart2(input)

		println("Part 1: " + part1)
		println("Part 2: " + part2)

	}
}
