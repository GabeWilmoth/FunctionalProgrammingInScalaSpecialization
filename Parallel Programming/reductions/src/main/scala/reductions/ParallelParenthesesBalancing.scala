package reductions

import scala.annotation.*
import org.scalameter.*

import scala.collection.mutable.{Map, Stack}

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:
  val openParan: Char = '('
  val closeParan: Char = ')'

  def update(char: Char, stack: Stack[Char]): Stack[Char] = {
    if (char == openParan) {
      stack.push(openParan)
    } else if (char == closeParan) {
      if (stack.nonEmpty && stack.head == openParan) {
        stack.pop()
      } else {
        stack.push(closeParan)
      }
    }
    stack
  }
    
  def getStack(chars: Array[Char], startIdx: Int, endIdx: Int): Stack[Char] = {
    var stack = new Stack[Char]()
    for (i <- startIdx until endIdx)
      stack = update(chars(i), stack)
    stack
  }

  def balance(chars: Array[Char]): Boolean =
    val stack = getStack(chars, 0, chars.length)
    stack.isEmpty

  def reduceStacks(bracketList: List[Char]): Stack[Char] = {
    var stack = new Stack[Char]()
    for (char <- bracketList) {
      stack = update(char, stack)
    }
    stack
  }

  def parTraverse(chars: Array[Char], threshold: Int, startIdx: Int, endIdx: Int): List[Char] = {
    val segmentLength = endIdx - startIdx
    val midPoint = startIdx + segmentLength / 2

    segmentLength match {
      case divide if (segmentLength > threshold) =>
        val parRes = parallel(
          parTraverse(chars, threshold, startIdx, midPoint),
          parTraverse(chars, threshold, midPoint, endIdx),
        )
        parRes._1.toList ++ parRes._2.toList
      case _ => getStack(chars, startIdx, endIdx).toList
    }
  }

  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    val joinedSegmentStacks = parTraverse(
      chars, threshold, 0, chars.length
    )

    val stack = reduceStacks(joinedSegmentStacks)
    stack.isEmpty
  }