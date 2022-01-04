package com.rockthejvm.lists

import scala.annotation.tailrec
import scala.util.Random

sealed abstract class RList[+T] {

  def head: T

  def tail: RList[T]

  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  def apply(index: Int): T

  def length: Int

  def reverse: RList[T]

  def ++[S >: T](anotherList: RList[S]): RList[S]

  def removeAt(index: Int): RList[T]

  // the big 3

  def map[S >: T](f: T => S): RList[S]

  def flatMap[S](f: T => RList[S]): RList[S]

  def filter(f: T => Boolean): RList[T]

  def rle: RList[(T, Int)]

  def duplicateEach(k: Int): RList[T]

  def rotate(k: Int): RList[T]

  // random sample
  def sample(k: Int): RList[T]
}

case object RNil extends RList[Nothing] {

  override def head: Nothing = throw new NoSuchElementException // side effect

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = throw new NoSuchElementException

  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  override def removeAt(index: Int): RList[Nothing] = throw new NoSuchElementException

  override def map[S](f: Nothing => S): RList[S] = RNil

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  // Medium difficulty

  override def rle: RList[(Nothing, Int)] = RNil

  override def duplicateEach(k: Int): RList[Nothing] = RNil

  override def rotate(k: Int): RList[Nothing] = RNil

  override def sample(k: Int): RList[Nothing] = RNil
}

// Cons ==  old name for Constructor
case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {

  override def isEmpty: Boolean = false

  override def toString: String = {

    @tailrec
    def toStringTailRec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailRec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailRec(this, "") + "]"
  }

  // When designing tailRec functions think about what is going to change over time as you iterate and use those as the function params
  /*
   What is the complexity of this tailRec function??
   Big O notation
   the method is O(min(N, index)), N being the dimension of the list, so it is the minimum of either the size of the list or the index you wish to search for
   */

  override def apply(index: Int): T = {

    @tailrec
    def applyTailRec(remaining: RList[T], currentIndex: Int): T = {
      if (currentIndex == index) remaining.head
      else applyTailRec(remaining.tail, currentIndex + 1)
    }

    if (index < 0) throw new NoSuchElementException
    else applyTailRec(this, 0)
  }

  // Big O(N)
  override def length: Int = {

    @tailrec
    def lengthTailRec(remainingList: RList[T], acc: Int): Int = {
      if (remainingList.isEmpty) acc
      else lengthTailRec(remainingList.tail, acc + 1)
    }

    lengthTailRec(this, 0)
  }

  // O(N)
  override def reverse: RList[T] = {

    @tailrec
    def reverseTailRec(remainingList: RList[T], result: RList[T]): RList[T] = {
      if (remainingList.isEmpty) result
      else reverseTailRec(remainingList.tail, remainingList.head :: result)
    }

    reverseTailRec(this, RNil)
  }

  override def ++[S >: T](anotherList: RList[S]): RList[S] = { //type S is used throughout

    @tailrec
    def concatTailRec(remainingList: RList[S], acc: RList[S]): RList[S] = {
      if (remainingList.isEmpty) acc
      else concatTailRec(remainingList.tail, remainingList.head :: acc)
    }

    concatTailRec(anotherList, this.reverse).reverse
  }

  override def removeAt(index: Int): RList[T] = {

    /* for index to remove at = 2, below is the algo of how the tailRec will evaluate
    [1,2,3,4,5].remove(2) = removeAtTailRec([1,2,3,4,5], 0, [])
    = removeAtTailRec([2,3,4,5], 1, [1])
    = removeAtTailRec([3,4,5], 2, [2,1])
    = [2,1].reverse ++ [4,5]
    */

    @tailrec
    def removeAtTailRec(remaining: RList[T], currentIndex: Int, predecessor: RList[T]): RList[T] = {
      if (currentIndex == index) predecessor.reverse ++ remaining.tail // you do not add the head here since at the index to remove you want to drop the head value
      else if (remaining.isEmpty) predecessor.reverse
      else removeAtTailRec(remaining.tail, currentIndex + 1, remaining.head :: predecessor)
    }

    removeAtTailRec(this, 0, RNil)
  }

  //  override def map[S](f: T => S): RList[S] = f(head) :: tail.map(f) // stack recursive so bad

  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def mapTailRec(remainingList: RList[T], acc: RList[S]): RList[S] = {
      /*
        [1,2,3].map(x => x + 1) = mapTailRec([1,2,3], [])
        = mapTailRec([2,3], [2])
        = mapTailRec([3], [3, 2])
        = mapTailRec([], [4 ,3, 2])
        Complexity = O(N)
      */

      if (remainingList.isEmpty) acc.reverse
      else mapTailRec(remainingList.tail, f(remainingList.head) :: acc)
    }

    mapTailRec(this, RNil)
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {

    @tailrec
    def flatMapTailRec(remainingList: RList[T], acc: RList[S]): RList[S] = {
      /*
        [1,2,3].flatMap(x => 2 * x) = flatMapTailRec([1,2,3], [])
        = flatMapTailRec([2,3], [1, 2].reverse)
        = flatMapTailRec([3], [2, 4].reverse ++ [1, 2].reverse)
        = flatMapTailRec([], [3 ,6].reverse ++ [2, 4].reverse ++ [1, 2].reverse)
        = [6,3,4,2,2,1].reverse
        = [1,2,3,4,3,6]
        Complexity = O(N) - Naive
        Complexity = O(sum of all the lengths of f(x) = Z) - Less Naive
        Complexity = O(Z^2) - True Complexity, so pretty bad
      */

      if (remainingList.isEmpty) acc.reverse
      else flatMapTailRec(remainingList.tail, f(remainingList.head).reverse ++ acc)
    }

    flatMapTailRec(this, RNil)
  }

  override def filter(predicate: T => Boolean): RList[T] = {
    @tailrec
    def filterTailRec(remainingList: RList[T], acc: RList[T]): RList[T] = {
      if (remainingList.isEmpty) acc.reverse
      else if (predicate(remainingList.head)) filterTailRec(remainingList.tail, remainingList.head :: acc)
      else filterTailRec(remainingList.tail, acc)
    }

    filterTailRec(this, RNil)
  }

  override def rle: RList[(T, Int)] = {

    /*
     [1,1,1,2,2,3,4,4,4,5].rle
     = rleTailRec([1,1,2,2,3,4,4,4,5], (1,1), [])
     = rleTailRec([1,2,2,3,4,4,4,5], (1, 2), [])
     = rleTailRec([2,2,3,4,4,4,5], (1, 3), [])
     = rleTailRec([2,3,4,4,4,5], (2, 1), [(1,3)])
     = rleTailRec([3,4,4,4,5], (2, 2), [(1,3)])
     = rleTailRec([4,4,4,5], (3, 1), [(2,2), (1,3)])
     = rleTailRec([4,4,5], (4, 1), [(3,1), (2,2), (1,3)])
     = ...
     = [(5,1), (4,3), (3,1), (2,2), (1,3)].reverse
     = [(1,3), (2,1), (3,2), (4,1), (5,3)]
     Complexity = O(N)
    */

    @tailrec
    def rleTailRec(remainingList: RList[T], currentTuple: (T, Int), acc: RList[(T, Int)]): RList[(T, Int)] = {
      if (remainingList.isEmpty && currentTuple._2 == 0) acc
      else if (remainingList.isEmpty) currentTuple :: acc
      else if (remainingList.head == currentTuple._1) rleTailRec(remainingList.tail, currentTuple.copy(_2 = currentTuple._2 + 1), acc)
      else rleTailRec(remainingList.tail, (remainingList.head, 1), currentTuple :: acc)
    }

    rleTailRec(this.tail, (this.head, 1), RNil).reverse
  }

  //  override def duplicateEach(k: Int): RList[T] = {
  //
  //    /* My implementation
  //     [1,2,3,4,5].duplicate(3)
  //     = duplicateTailRec([1,2,3,4,5], (1,0), [])
  //     = duplicateTailRec([1,2,3,4,5], (1,1), [1])
  //     = duplicateTailRec([1,2,3,4,5], (1,2), [1,1])
  //     = duplicateTailRec([1,2,3,4,5], (1,3), [1,1,1])
  //     = duplicateTailRec([2,3,4,5], (2,0), [1,1,1])
  //     = duplicateTailRec([2,3,4,5], (2,1), [2,1,1,1])
  //     = duplicateTailRec([2,3,4,5], (2,2), [2,2,1,1,1])
  //     = duplicateTailRec([2,3,4,5], (2,3), [2,2,2,1,1,1])
  //     = duplicateTailRec([3,4,5], (3,0), [2,2,2,1,1,1])
  //     = duplicateTailRec([3,4,5], (3,1), [3,2,2,2,1,1,1])
  //     = duplicateTailRec([3,4,5], (3,2), [3,3,2,2,2,1,1,1])
  //     = duplicateTailRec([3,4,5], (3,3), [3,3,3,2,2,2,1,1,1])
  //     = ...
  //     = [5, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1].reverse
  //     = [1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5]
  //     Complexity = O(N)
  //    */
  //
  //    @tailrec
  //    def duplicateTailRec(remainingList: RList[T], counter: (T, Int), acc: RList[T]): RList[T] = {
  //      if (remainingList.isEmpty && counter._2 == 0) acc
  //      else if (remainingList.isEmpty) acc
  //      else if (counter._2 != k) {
  //        duplicateTailRec(remainingList, counter.copy(_1 = remainingList.head, _2 = counter._2 + 1), remainingList.head :: acc)
  //      }
  //      else {
  //        duplicateTailRec(remainingList.tail, (remainingList.head, 0), acc)
  //      }
  //    }
  //
  //    duplicateTailRec(this, (this.head, 0), RNil).reverse
  //  }

  override def duplicateEach(k: Int): RList[T] = {

    @tailrec
    def duplicateTailRec(remaining: RList[T], currentElement: T, nDuplications: Int, acc: RList[T]): RList[T] = {
      if (remaining.isEmpty && nDuplications == k) acc
      else if (remaining.isEmpty) duplicateTailRec(remaining, currentElement, nDuplications + 1, currentElement :: acc)
      else if (nDuplications == k) duplicateTailRec(remaining.tail, remaining.head, 0, acc)
      else duplicateTailRec(remaining, currentElement, nDuplications + 1, currentElement :: acc)
    }

    duplicateTailRec(this.tail, this.head, 0, RNil).reverse
  }

  override def rotate(k: Int): RList[T] = {
    def rotateTailRec(remainingList: RList[T], counter: Int, acc: RList[T]): RList[T] = {
      if (remainingList.isEmpty && counter == 0) this
      else if (remainingList.isEmpty) rotateTailRec(this, counter, RNil)
      else if (counter == 0) remainingList ++ acc.reverse
      else rotateTailRec(remainingList.tail, counter - 1, remainingList.head :: acc)
    }

    rotateTailRec(this, k, RNil)
  }

  override def sample(k: Int): RList[T] = {

    val randomNumber = new Random(System.currentTimeMillis())
    val maxIndex = this.length

    // Complexity O(N * k)
    def sampleTailRec(nRemaining: Int, acc: RList[T]): RList[T] = {

      if (nRemaining == 0) acc.reverse
      else {
        val index = randomNumber.nextInt(maxIndex)
        val newNumber = this.apply(index)
        sampleTailRec(nRemaining - 1, newNumber :: acc)
      }
    }

    def sampleElegant: RList[T] =
      RList.from((1 to k).map { _ => randomNumber.nextInt(maxIndex)
      }.map { index => this (index) })

    def sampleElegantLens: RList[T] = {
      val genSeq: Seq[Int] = (1 to k)   // gens up to k to determine number of elems to sample from original list
      val genSeqOfRandomIndexes: Seq[Int] = genSeq.map { _ => randomNumber.nextInt(maxIndex) }
      // uses the range above to gen the random indexes using our "val randomNumber = new Random(System.currentTimeMillis())"  random generator to choose our random samples
      val genFinalListOfSampledElements: Seq[T] = genSeqOfRandomIndexes.map { index => this (index) } // Gives us a Seq/Vector of Randomly sampled elems

      println(genSeq)
      println(genSeqOfRandomIndexes)
      println(genFinalListOfSampledElements)

      RList.from(genFinalListOfSampledElements) // converts the Vector type to RList type
    }

    if (k < 0) RNil
    else sampleElegantLens //sampleTailRec(k, RNil)
  }


}

object RList {

  def from[T](iterable: Iterable[T]) = {
    def convertToRListTailRec(remaining: Iterable[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else convertToRListTailRec(remaining.tail, remaining.head :: acc)
    }

    convertToRListTailRec(iterable, RNil).reverse
  }
}

object ListProblems extends App {

  val aSmallList: ::[Int] = ::(1, ::(2, ::(3, RNil)))
  val aSmallList2: RList[Int] = 1 :: 2 :: 3 :: RNil // Right associative

  val aList = 1 :: 2 :: 3 :: 4 :: 5 :: 8 :: 99 :: RNil
  val aLargeList = RList.from(1 to 10000)

  def testEasyProblems = {

    //  println(aSmallList)

    //  println(aSmallList.apply(0))
    //  println(aSmallList.apply(2))

    //  println(aList.length)
    //  println(aList.reverse)

    //  println(aLargeList.apply(8735))
    //  println(aLargeList.reverse)
    //  println(aSmallList ++ aLargeList)

    //  println(aList.removeAt(4)) // '5' gets removed so new list = [1,2,3,4,8,99]
    //  println(aList.map(x => 2 * x))

    //  val time = System.currentTimeMillis()
    //  println(aLargeList.flatMap(x => x :: (2 * x) :: RNil)) // 2.8s to evaluate so pretty bad
    //  println(System.currentTimeMillis() - time)
  }

  // Medium
  def testMedium = {

    //    val duplicatesList: RList[Int] = 1 :: 1 :: 1 :: 2 :: 3 :: 3 :: 4 :: 5 :: 5 :: 5 :: RNil
    //    println(duplicatesList.rle)
    //
    //    val aList: RList[Int] = 1 :: 2 :: 3 :: 4 :: 5 :: RNil
    //    println(aList.duplicateEach(3))
    //
    val aList1To10: RList[Int] = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: RNil
    //    println(aList1To10.rotate(3))

    println(aList1To10.sample(3))
  }

  testMedium
}
