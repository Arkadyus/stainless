import stainless.lang._
import stainless.collection._
import stainless.lang.Option._
import stainless.annotation._
import stainless.proof._
import stainless.lang.StaticChecks._

object Queue {
  final case class Node(val value: BigInt, var nextOpt: Option[Node]) extends AnyHeapRef {}

  final case class Q(var first: Node,
                     var last: Node,
                     @ghost var size: BigInt,
                     @ghost var nodes: List[AnyHeapRef])
             extends AnyHeapRef
  {
    // first is a sentinel node, not stored in nodes

    @ghost
    def valid: Boolean = {      
      reads(nodes.content ++ Set(this, first))
      size == nodes.size &&
      !nodes.contains(first) &&
      inv(nodes, first.nextOpt)
    }

    @ghost
    def inv(nodesLeft: List[AnyHeapRef], current: Option[Node]): Boolean = {
      reads(nodesLeft.content ++ Set(this))
      decreases(nodesLeft.size)
      
      nodesLeft match {
        case Cons(hh, tail) => {
          hh.isInstanceOf[Node] &&
          ({
            val h = hh.asInstanceOf[Node]
            current == Some(h)  &&  inv(tail, h.nextOpt)
          })
        }
        case Nil() => current==None()
      }
    }    
  
    def enqueue(n: Node): Unit = {
      reads(nodes.content ++ Set(this, first))
      require(valid && !nodes.contains(n))
      modifies(Set(this, last))

      last.nextOpt = Some(n)
      last = n
      nodes = nodes ++ List(n)
      size = size + 1
    } ensuring (_ => valid)

    def dequeue: Option[BigInt] = {
      reads(nodes.content ++ Set(this, first, first.nextOpt.get))
      require(first.nextOpt != None() && size > 0 && valid)
      modifies(Set(this))

      first.nextOpt match {
        case Some(nn) => {
          first = nn
          size = size - 1
          nodes = nodes.tail
          Some(nn.value)
        }
        case _ => None[BigInt]()
      }
    } ensuring ((res:Option[BigInt]) => valid)
  }

  @extern
  def main(args: Array[String]): Unit = {
    val n = Node(-1, None[Node]())
    val q = Q(n, n, 0, List[AnyHeapRef]())
    println("Q with nodes")
    q.enqueue(Node(5, None[Node]()))
    q.enqueue(Node(10, None[Node]()))    
    q.enqueue(Node(14, None[Node]()))
    println(q.dequeue.get)
    println(q.dequeue.get)
    println(q.dequeue.get)
  }
  
}