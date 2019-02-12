object Test {

  def assert[T](name: String)(pred: () => Boolean) =
    if (pred()) {
      println(s"$name:PASS")
    } else {
      println(s"$name:FAIL")
    }

  def split[T](g: Graph[T], start: T, toCut: Map[T, Boolean]) = {
    val deps = g.edges(start).filter(!toCut(_))
  }
}

import Test._

case class Graph[A](edges: Map[A, Set[A]])

object Graph {
  def empty[A]() = Graph[A](Map())
}

object Main {

  def main(args: Array[String]) {
    assert("nothing")(() => 1 == 1)

//    assert("empty") { () =>
//      val cons = Graph.empty[Int]
//      split(cons,1, Map()) == Graph.empty[Int]
//    }

    assert("single element") { () =>
      val cons = Graph(Map(1 -> Set.empty[Int]))
      split(cons, 1, Map(1 -> true)) == cons
    }

    assert("two separate elements") { () =>
      val cons = Graph(Map(1 -> Set(2), 2 -> Set.empty[Int]))
      split(cons, 1, Map(1 -> true, 2 -> true)) == cons
    }

    assert("two related elements") { () =>
      val input  = Graph(Map(1 -> Set(2), 2 -> Set.empty[Int]))
      val output = Graph(Map(1 -> Set.empty[Int]))
      split(input, 1, Map(1 -> true, 2 -> false)) == output
    }

  }
}
