/**
 * https://adventofcode.com/2020/day/18
 */

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

object day18 {
	def main(args: Array[String]): Unit = {
		val source = Source.fromFile(args(0))
		var part1 = 0L
		var part2 = 0L

		val input = (for (line <- source.getLines()) yield line.replaceAll(" ", "").toList).toList
		val intPattern = "(\\d)".r

		// Use the Shunting-Yard algorithm to create a Reverse Polish Notation of the expression
		def rpnPart1(op: List[Char], output: String = "", stack: Stack[Char] = Stack[Char]()): String = op match {
			case Nil => output + stack.mkString
			case h :: t => h match {
				case intPattern(i) 	=> rpnPart1(t, output + i, stack)
				case ')' => {
					var acc = ""
					while (stack.top != '(')
						acc += stack.pop()
					stack.pop()
					rpnPart1(t, output + acc, stack)
				}
				case _ => rpnPart1(t, output, stack.push(h))
			}
		}

		// Use the Shunting-Yard algorithm to create a Reverse Polish Notation of the expression
		def rpnPart2(op: List[Char], output: String = "", stack: Stack[Char] = Stack[Char]()): String = op match {
			case Nil => output + stack.mkString
			case h :: t => h match {
				case intPattern(i) 	=> {
					// println(f"Case: $i, output: $output, stack: $stack")
					rpnPart2(t, output + i, stack)
				}
				case ')' => {
					var acc = ""
					// println(f"Case: ), output: $output, stack: $stack")
					while (stack.top != '(')
						acc += stack.pop()
					stack.pop()
					rpnPart2(t, output + acc, stack)
				}
				case '*' if (stack.nonEmpty && stack.top == '+') => {
					var acc = ""
					// println(f"Case: $h && top = +, output: $output, stack: $stack")
					while (stack.nonEmpty && stack.top == '+')
						acc += stack.pop()
					rpnPart2(t, output + acc, stack.push(h))
				}
				case x => {
					// println(f"Case: $x, output: $output, stack: $stack")
					rpnPart2(t, output, stack.push(x))
				}
			}
		}

		/**
		  * Execute the postfix expression by,
		  * once an operator is found,
		  * popping the top two values and evaluating them
		  *
		  */
		def execute(expression: List[Char], acc: Stack[Long] = Stack[Long]()): Long = expression match {
			case Nil => acc.sum
			case h :: t => h match {
				case intPattern(i) => execute(t, acc.push(Integer.parseInt(i.toString()).toLong))
				case '+' => {
					val (a, b) = (acc.pop(), acc.pop())
					val result = a + b
					execute(t, acc.push(result))
				}
				case '*' => {
					val (a, b) = (acc.pop(), acc.pop())
					val result = a * b
					execute(t, acc.push(result))
				}
			}
		}

		val (sum1, sum2) = (ListBuffer[Long](),ListBuffer[Long]())

		for (op <- input) {
			// Reverse the input expression to preserve "left-to-right" evaluation order
			val rev = op.reverse.map{
					case '(' => ')'
					case ')' => '('
					case x => x
				}
			val pfnPart1 = rpnPart1(rev).toList
			val pfnPart2 = rpnPart2(rev).toList
			// println(pfnPart2.mkString)
			sum1 += execute(pfnPart1)
			sum2 += execute(pfnPart2)
		}

		part1 = sum1.sum
		part2 = sum2.sum

		println("Part 1: " + part1)
		println("Part 2: " + part2)
	}
}
