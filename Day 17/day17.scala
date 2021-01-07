/**
 * https://adventofcode.com/2020/day/17
 */

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer

object day17 {
	def main(args: Array[String]): Unit = {
		val source = Source.fromFile(args(0))
		var part1 = 0
		var part2 = 0

		// [z, y, x]
		// [depth, row, col]
		def expandSpace3D(space: List[List[List[Char]]]): List[List[List[Char]]] = {

			// Fill a plane along the depth-axis
			val row = List.fill(space.head.length + 2)('.')
			val rowCol = List.fill(space.head.head.length + 2)(row)

			// Increase the current planes with 1 in each direction, and
			// add the two new rows above and below the current ones
			val padded = space.map(depthPlane => row +: depthPlane.map('.' +: _ :+ '.') :+ row)

			// Add the new depth planes in front and behind the current planes
			val result = rowCol +: padded :+ rowCol

			result
		}
		// [w, z, y, x]
		// [hyper, depth, row, col]
		def expandSpace4D(space: List[List[List[List[Char]]]]): List[List[List[List[Char]]]] = {

			// Expand the current 3D slices
			val expanded3D = space.map(expandSpace3D)

			// Create slices in each dimension
			val slice1D = List.fill(space.head.head.length + 2)('.')
			val slice2D = List.fill(space.head.head.head.length + 2)(slice1D)
			val slice3D = List.fill(space.head.length + 2)(slice2D)
			val slice4D = List.fill(space.length + 2)(slice3D)

			// Add 3D-slices to the current 4D space
			val result = slice3D +: expanded3D :+ slice3D

			result
		}

		def cycle3D(space: List[List[List[Char]]]): Int = {
			var prev = space
			for (i <- 0 until 6) {
				prev =
				(for (depth <- 0 until prev.length) yield
					(for (row <- 0 until prev(depth).length) yield
						(for (col <- 0 until prev(depth)(row).length) yield {

							// Find the surrounding active cubes
							val cubes = (for (x <- -1 to 1; y <- -1 to 1; z <- -1 to 1
											if !(x == 0 && y == 0 && z == 0);
											if depth + z >= 0 && depth + z < prev.length;
											if row   + y >= 0 && row   + y < prev(depth).length;
											if col   + x >= 0 && col   + x < prev(depth)(row).length
											) yield prev(depth+z)(row+y)(col+x)
										).filter(_ == '#').length

							// yield the new state with respect to the rules
							prev(depth)(row)(col) match {
								case '#' if (cubes != 2 && cubes != 3)  => '.'
								case '.' if (cubes == 3) 				=> '#'
								case x 									=> x
							}
						}).toList
					).toList
				).toList

				prev = expandSpace3D(prev)
			}
			// Return the number of cubes
			prev.flatten.flatten.filter(_ == '#').length
		}

		def cycle4D(space: List[List[List[List[Char]]]]): Int = {
			var prev = space
			for (i <- 0 until 6) {
				prev =
				(for (hyper <- 0 until prev.length) yield
					(for (depth <- 0 until prev(hyper).length) yield
						(for (row <- 0 until prev(hyper)(depth).length) yield
							(for (col <- 0 until prev(hyper)(depth)(row).length) yield {
								// println(f"Hyper: $hyper, Depth: $depth, Row: $row, Col: $col")
								// Find the surrounding active cubes
								val cubes = (for (x <- -1 to 1; y <- -1 to 1; z <- -1 to 1; w <- -1 to 1;
												if !(x == 0 && y == 0 && z == 0 && w == 0);
												if hyper + w >= 0 && hyper + w < prev.length;
												if depth + z >= 0 && depth + z < prev(hyper).length;
												if row   + y >= 0 && row   + y < prev(hyper)(depth).length;
												if col   + x >= 0 && col   + x < prev(hyper)(depth)(row).length
												) yield {
													// println(f"x: $x, y: $y, z: $z, w: $w")
													prev(hyper+w)(depth+z)(row+y)(col+x)
												}
											).filter(_ == '#').length

								// yield the new state with respect to the rules
								prev(hyper)(depth)(row)(col) match {
									case '#' if (cubes != 2 && cubes != 3)  => '.'
									case '.' if (cubes == 3) 				=> '#'
									case x 									=> x
								}
							}).toList
						).toList
					).toList
				).toList

				prev = expandSpace4D(prev)
			}
			// Return the number of cubes
			prev.flatten.flatten.flatten.filter(_ == '#').length
		}

		// Input stuff
		val rawInput = (for (line <- source.getLines()) yield line.toList).toList
		val row = List.fill(rawInput.head.length + 2)('.')
		val rowCol = List.fill(rawInput.length + 2)(row)
		val boringSpace = row +: rawInput.map('.' +: _ :+ '.') :+ row
		val input3D = List(rowCol, boringSpace, rowCol)
		val hyperSpace = List.fill(input3D.length + 2)(rowCol)
		val input4D = List(hyperSpace, rowCol +: input3D :+ rowCol, hyperSpace)

		part1 = cycle3D(input3D)
		part2 = cycle4D(input4D)

		println("Part 1: " + part1)
		println("Part 2: " + part2)
	}
}
