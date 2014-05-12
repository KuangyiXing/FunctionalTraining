package com.rea.higherorder

/*
 * DO NOT ATTEMPT these exercises until you've completed the recursion ones.
 */

object FoldingExercises {

  /**
   *
   * foldLeft will reduce a list of A's down to a B. It takes an initial value of type B
   * and a list of A's.  It also takes a function which takes the accumulated value of type B
   * and the next value in the list (of type A) and returns a value which will be feed back into
   * the accumulator of the next call.
   *
   * As the name suggests it processes the list from left to right.
   *
   * Have a close look at your implementations from the RecursionExercises.  Which parts could you
   * pull out to a function to make them all common?  Your implementation will be very close to
   * foldLeft.
   *
   * Good luck!
   *
   */
  def foldLeft[A, B](initialValue: B, list: List[A])(f: (B, A) => B): B = ???

  /**
   * foldRight is the same as foldLeft, except it processes the list from right to left.
   */
  def foldRight[A,B](initialValue:B, list: List[A])(f: (A,B) => B):B = ???

  /**
   * Remember these, from our recursion exercises?  They can all be implemented with either
   * foldLeft or foldRight.
   */

  def sum(l: List[Int]): Int = ???

  def length[A](x: List[A]): Int = ???

  def map[A, B](x: List[A])(f: A => B): List[B] = ???

  def filter[A](x: List[A], f: A => Boolean): List[A] = ???

  def append[A](x: List[A], y: List[A]): List[A] = ???

  def flatten[A](x: List[List[A]]): List[A] = ???

  def flatMap[A, B](x: List[A], f: A => List[B]): List[B] = ???

  // Maximum of the empty list is 0
  def maximum(x: List[Int]): Int = ???

  def reverse[A](x: List[A]): List[A] = ???

  def main(args: Array[String]) = {
    assert(foldLeft(0, List(1,2,3))(_+_) == 6)
    assert(foldLeft(List[Int](), List(1,2,3))((a,e) =>e :: a) == List(3,2,1))
    assert(foldRight(List[Int](), List(1,2,3))((e,a) =>e :: a) == List(1,2,3))
    assert(foldRight(0, List(1,2,3))(_+_) == 6)

    println("Sum of List(1,2,3,4,5,6) = 21: " + sum(List(1, 2, 3, 4, 5, 6)))
    println("Length of List(1,2,3,4,5,6) = 6: " + length(List(1, 2, 3, 4, 5, 6)))

    println("Add one to List(1,2,3,4,5,6) = List(2,3,4,5,6,7): " + map(List(1, 2, 3, 4, 5, 6))(_+1))

    println("Remove elements under 4 for List(1,2,3,4,5,6) = List(4,5,6): " + filter(List(1, 2, 3, 4, 5, 6), {
      x: Int => x >= 4
    }))

    println("Append List(a,b,c) with List(d,e,f) = List(a,b,c,d,e,f): " + append(List('a', 'b', 'c'), List('d', 'e', 'f')))

    println("Flatten a List(List(a,b,c),List(e,f,g), List(h,i,j)) = List(a,b,c,d,e,f,g,h,i,j): " + flatten(
      List(List('a', 'b', 'c'), List('d', 'e', 'f'), List('h', 'i', 'j'))))

    println("Run a flatMap over List(hello, world) with function split = List(h,e,l,l,o,w,o,r,l,d): " + flatMap(List("hello", "world"), (_: String).toCharArray.toList))

    println("maxium of List(4,3,5,7,1,2,6,3,4,5,6) = 7: " + maximum(List(4, 3, 5, 7, 1, 2, 6, 3, 4, 5, 6)))

    println("Reverse a List(1,2,3,4,5,6) = List(6,5,4,3,2,1) : " + reverse(List(1,2,3,4,5,6)))
  }

}