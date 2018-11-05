package com.rea.higherorder

import com.rea.typesafety.OptionalExercises3.Nothing

/**
  * DO NOT ATTEMPT these exercises until you've completed the recursion ones.
  *
  * foldLeft will reduce a list of A's down to a B. It takes an initial value of type B
  * and a list of A's.  It also takes a function which takes the accumulated value of type B
  * and the next value in the list (of type A) and returns a value which will be feed back into
  * the accumulator of the next call.
  *
  * As the name suggests it processes the list from left to right.
  *
  * Have a close look at your implementations from the RecursionExercises.  Which parts could you
  * pull out to a function to make them all common? Your implementation will be very close to
  * foldLeft.
  *
  * Good luck!
  *
  */

object FoldingExercises {

  def foldLeft[A, B](initialValue: B, list: List[A])(f: (B, A) => B): B = {
    def go(total: B, l: List[A]): B = l match {
      case List() => total
      case head :: tail => go(f(total, head), tail)
    }

    go(initialValue, list)
  }

  /**
    * foldRight is the same as foldLeft, except it processes the list from right to left.
    */
  def foldRight[A, B](initialValue: B, list: List[A])(f: (A, B) => B): B = {
    def go(total: B, l: List[A]): B = l match {
      case List() => total
      case head :: tail => go(f(head, total), tail)
    }

    go(initialValue, list)
  }

  /**
    * Remember these, from our recursion exercises?  They can all be implemented with either
    * foldLeft or foldRight.
    */

  def sum(x: List[Int]): Int = {
    foldLeft[Int, Int](0, x)(_ + _)
  }

  def length[A](x: List[A]): Int = {
    foldLeft[A, Int](0, x)((length, _) => length + 1)
  }

  //Careful you'll need a type annotation on the initialValue field
  def map[A, B](x: List[A])(f: A => B): List[B] = {
    foldLeft[A, List[B]](List(), x)((l, a) => l :+ f(a))
  }

  def filter[A](x: List[A], f: A => Boolean): List[A] = {
    foldLeft[A, List[A]](List(), x)((l, a) => {
      if (f(a))
        l:+a
      else
        l
    }
    )
  }

  def append[A](x: List[A], y: List[A]): List[A] = {
    foldLeft[A,List[A]](x,y)((l,a) => l:+a)
  }

  def flatten[A](x: List[List[A]]): List[A] = {
    foldLeft[List[A],List[A]](List(),x)((b,a) => append(b,a))
  }

  def flatMap[A, B](x: List[A], f: A => List[B]): List[B] = {
    foldLeft[A,List[B]](List(),x)((l,a) => append(l, f(a)))
  }

  // Maximum of the empty list is 0
  def maximum(x: List[Int]): Int = {
    def bigger(a: Int,b: Int): Int = if (a > b) a else b
    foldLeft[Int,Int](0,x)(bigger)
  }


  def reverse[A](x: List[A]): List[A] = {
    foldLeft[A, List[A]](List(), x)((l,a) => a::l)
  }
}
