package fury

object Test {

  def assert[T](name: String)(pred: () => Boolean) =
    if (pred()) {
      println(s"$name:PASS")
    } else {
      println(s"$name:FAIL")
    }

  def equ[T](lhs: T, rhs: T) =
    if (lhs == rhs) true
    else {
      println(s"LHS: ${lhs}")
      println(s"RHS: ${rhs}")
      false
    }
}

import Test._
case class DirGraph[A](edges: Map[A, Set[A]]) {

  def reduce(element: A): DirGraph[A] = {
    val pointingTo = edges(element)
    val noFromEdge = edges - element
    DirGraph(noFromEdge.mapValues { x =>
      if (x(element)) x ++ pointingTo - element else x
    })
  }

  def subgraph(toLeave: Set[A]): DirGraph[A] = {
    val toCut = edges.keySet &~ toLeave
    toCut.foldRight(this) { (element, graph) =>
//      println (s"g: ${graph}")
      graph.reduce(element)
    }
  }
}

object DirGraph {
  def empty[A]() = DirGraph[A](Map())
}

object MainGraphTest {

  def main(args: Array[String]) {
    assert("nothing")(() => 1 == 1)

    assert("single element") { () =>
      val cons = DirGraph(Map(1 -> Set.empty[Int]))
      cons.subgraph(Set(1)) == cons
    }

    assert("two separate elements") { () =>
      val cons = DirGraph(Map(1 -> Set(2), 2 -> Set.empty[Int]))
      cons.subgraph(Set(1, 2)) == cons
    }

    assert("two related elements") { () =>
      val input  = DirGraph(Map(1 -> Set(2), 2 -> Set.empty[Int]))
      val output = DirGraph(Map(1 -> Set.empty[Int]))
      input.subgraph(Set(1)) == output
    }

    assert("remove two elements") { () =>
      val input  = DirGraph(Map(1 -> Set(2), 2 -> Set(3), 3 -> Set.empty[Int]))
      val output = DirGraph(Map(1 -> Set.empty[Int]))
      input.subgraph(Set(1)) == output
    }

    assert("remove one element") { () =>
      val input  = DirGraph(Map(1 -> Set(2), 2 -> Set(3), 3 -> Set.empty[Int]))
      val output = DirGraph(Map(1 -> Set(2), 2 -> Set.empty[Int]))
      input.subgraph(Set(1, 2)) == output
    }

    assert("remove one element") { () =>
      val input  = DirGraph(Map(1 -> Set(2, 4), 2 -> Set(3, 4), 3 -> Set(4), 4 -> Set.empty[Int]))
      val output = DirGraph(Map(1 -> Set(2), 2    -> Set.empty[Int]))
      equ(input.subgraph(Set(1, 2)), output)
    }
  }
}
