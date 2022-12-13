/* Copyright 2009-2021 EPFL, Lausanne */

package stainless
package verification

/**
 * Transform trees by inserting Reachability probes. s is the input tree, t is the transformed tree
 */
class ReachabilityProbeInjector(override val s: ast.Trees, override val t: ast.Trees)
                       (using val symbols: s.Symbols)
  extends transformers.ConcreteTreeTransformer(s, t) {
    override def transform(e: s.Expr): t.Expr = e match {
        // can the pattern have side effects ?
        case s.MatchExpr(scrutinee, cases) => t.MatchExpr(transform(scrutinee), cases.map(casee => casee match {
            case s.MatchCase(pattern, optGuard, rhs) => t.MatchCase(pattern, transform(optGuard), t.ReachabilityProbe(transform(rhs)))
        }))

        case s.IfExpr(cond, thenn, elze) => t.IfExpr(transform(cond), t.ReachabilityProbe(transform(thenn)), t.ReachabilityProbe(transform(elze)))

        case _ => super.transform(e) // go through the other expressions and transform recursively

    }
}

object ReachabilityProbeInjector { self =>
  def apply(tr: Trees)(using inox.Context): extraction.ExtractionPipeline {
    val s: tr.type
    val t: tr.type
  } = {
    class Impl(override val s: tr.type, override val t: tr.type) extends ReachabilityProbeInjector(s, t)
    new Impl(tr, tr)
  }
}
