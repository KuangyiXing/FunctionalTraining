package com.rea.recursion

// Taken from http://tmorris.net/posts/scala-exercises-for-beginners/index.html

/**
  * Ok here are the rules.
  *
  * You can't use any of the standard list functions, like `map`, `filter`, `flatMap`, `append`, `:::`, `:+`, etc.
  *
  * But you can always use `::` to construct a new list by prepending an element to another list.
  *
  * You CAN and are encouraged to use the solutions from the exercises below to solve the harder
  * ones towards the end.
  *
  * Keep an eye out for repetition and similarities between your answ
  *
  * REMEMBER: Follow the types, they almost always guide you to the solution.  If it compiles and looks a little
  * too simple, it's probably correct.  As Sherlock Holmes once said, "Each one is suggestive, together they are
  * most certainly conclusive.:S#_NAME
  *
  * See if you can make your solution tail recursive, where possible.
  *
  */

object RecursionExercises {

  def plusOne(n: Int) = n + 1

  def minusOne(n: Int) = n - 1

  // Add two non-negative Integers together.  You are only allowed to use plusOne and minusOne above
  def add(a: Int, b: Int): Int = b match {
    case 0 => a
    case _ => add(plusOne(a), minusOne(b))
  }

  // You are not permitted to use any list functions such as map, flatMap, ++, flatten etc
  def sum(l: List[Int]): Int = {
    def kuangyi(total: Int, x: List[Int]): Int = {
      x match {
        case List() => total
        case head :: tail => kuangyi(total + head, tail)
      }
    }

    kuangyi(0, l)
  }

  //Again no list functions are permitted for the following
  def length[A](x: List[A]): Int = {
    def go(total: Int, l: List[A]): Int = {
      l match {
        case List() => total
        case head :: tail => go(total + 1, tail)
      }
    }

    go(0, x)
  }

  // Do you notice anything similar between sum and length? Hmm...

  // Mapping over a list.  You are given a List of type A and a function converting an A to a B
  // and you give back a list of type B.  No list functions allowed!
  //  def map[A, B](x: List[A], f: A => B): List[B] = x match {
  //    case List() => Nil
  //    case head::tail => f(head)::map(tail,f)
  //  }

  //  def map[A, B](x: List[A], f: A => B): List[B] = x match {
  //    case List() => Nil
  //    case head::tail => f(head)::map(tail,f)
  //  }
  def map[A, B](x: List[A], f: A => B): List[B] = {
    def go(total: List[B], l: List[A]): List[B] = {
      l match {
        case List() => total
        case head :: tail => go(f(head) :: total, tail)
      }
    }

    go(List(), x)
  }

  // Given a function from A => Boolean, return a list with only those item where the function returned true.
  //  def filter[A](x: List[A], f: A => Boolean): List[A] = x match {
  //    case List() => Nil
  //    case head::tail => if (f(head)) head::filter(tail, f) else filter(tail, f)
  //  }
  def filter[A](x: List[A], f: A => Boolean): List[A] = {
    def go(total: List[A], l: List[A]): List[A] = l match {
      case List() => total
      case head :: tail => if (f(head)) go(head :: total, tail) else go(total, tail)
    }

    go(List(), x)
  }

  // This pattern should be familiar by now... psst... look at add.
  //  def append[A](x: List[A], y: List[A]): List[A] = x match {
  //    case List() => y
  //    case head :: tail => head :: append(tail, y)
  //  }

  def append[A](x: List[A], y: List[A]): List[A] = {
    def go(total: List[A], l: List[A]): List[A] = l match {
      case List() => total
      case head :: tail => go(head :: total, tail)
    }

    go(y, x)
  }

  // Flatten a list of lists to a single list.  Remember you can't use list.flatten.  Can you use a previous
  // solution to solve this one?
  //  def flatten[A](x: List[List[A]]): List[A] = x match {
  //    case List() => Nil
  //    case head :: tail => append(head, flatten(tail))
  //  }

  def flatten[A](x: List[List[A]]): List[A] = {
    def go(total: List[A], l: List[List[A]]): List[A] = l match {
      case List() => total
      case head::tail => go(append(head,total), tail)
    }

    go(List(), x)
  }


  // Follow the types.  You've done a great job getting here. Follow the types.
//  def flatMap[A, B](x: List[A], f: A => List[B]): List[B] = x match {
//    case List() => Nil
//    case head :: tail => append(f(head), flatMap(tail, f))
//  }

  def flatMap[A, B](x: List[A], f: A => List[B]): List[B] = {
    def go(total: List[B], l: List[A]):List[B] = l match {
      case List() => total
      case head::tail => go(append(f(head),total), tail)
    }
    go(List(),x)
  }

  // Maximum of the empty list is 0
//  def maximum(x: List[Int]): Int = x match {
//    case List() => 0
//    case head :: tail => if (head > maximum(tail)) head else maximum(tail)
//  }

  def maximum(x: List[Int]): Int = {
    def go(total:Int, l: List[Int]): Int = l match {
      case List() => total
      case head::tail => if (head > total) go(head,tail) else go(total,tail)
    }

    go(0, x)
  }

  // Reverse a listbnmbn
//  def reverse[A](x: List[A]): List[A] = x match {
//    case List() => Nil
//    case head :: tail => append(reverse(tail), List(head))
//  }

  def reverse[A](x: List[A]): List[A] = {
    def go(total: List[A], l: List[A]): List[A] = l match {
      case List() => total
      case head::tail => go(head::total, tail)
    }

    go(List(), x)
  }
}
