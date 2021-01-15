import stainless.lang._
import stainless.collection._
import stainless.lang.Option._
import stainless.annotation._
import stainless.proof.check
import stainless.lang.StaticChecks._

object Stack {
  final case class Node(val value: BigInt, var nextOpt: Option[Node]) extends AnyHeapRef {}

  final case class Q(var first: Option[Node],
                     @ghost var nodes: List[AnyHeapRef])
             extends AnyHeapRef
  {
    // first is a sentinel node, not stored in nodes
    
    @ghost
    def valid: Boolean = {      
      reads(nodes.content ++ Set(this))
      inv(nodes, first)
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
        case Nil() => current==None[Node]()
      }
    }

    /*
    @ghost
    def invTailLemma(nodesLeft: List[AnyHeapRef], current: Option[Node]): Unit = {
      reads(nodesLeft.content ++ Set(this))
      require(inv(nodesLeft, current) && nodesLeft != Nil[AnyHeapRef]() && current != None[Node]())
      decreases(nodesLeft.size)

      ()
    } ensuring(_ => inv(nodesLeft.tail, current.get.nextOpt))
*/

  
    def push(n: Node): Unit = {
      reads(nodes.content ++ Set(this, n))
      require(inv(nodes, first) && !nodes.contains(n))
      modifies(Set(this, n))

      n.nextOpt = first
      first = Some(n)
      nodes = n :: nodes 
    } ensuring (_ => inv(nodes, first))

    /*
    def pop: BigInt = {
      reads(nodes.content ++ Set(this))
      require(valid && !nodes.isEmpty)
      modifies(Set(this))

      @ghost val ok1 = invTailLemma(nodes, first)
      val n = first.get
      first = n.nextOpt
      size = size - 1
      nodes = nodes.tail
      @ghost val ok2 = check(inv(nodes, first))
      n.value
    } ensuring (_ => valid && nodes == old(this.nodes.tail))
    */
  }

  @extern
  def main(args: Array[String]): Unit = {
    val n = Node(-1, None[Node]())
    val s = Q(None[Node], List[AnyHeapRef]())
    println("Stack with nodes")
    s.push(Node(5, None[Node]()))
    s.push(Node(10, None[Node]()))    
    s.push(Node(14, None[Node]()))
    println("Stack is: " + s)
    /*
    println(s.pop)
    println(s.pop)
    println(s.pop)
    println("Stack is: " + s)
    */
  }
  
}
