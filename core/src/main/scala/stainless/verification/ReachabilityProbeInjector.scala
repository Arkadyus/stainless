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
        case s.MatchCase(pattern, optGuard, rhs) => t.MatchCase(pattern, optGuard, t.Seq(ReachabilityProbe, rhs))

        case s.IfExpr(cond, thenn, elze) => t.IfExpr(cond, t.Seq(ReachabilityProbe, thenn), t.Seq(ReachabilityProbe, elze))

    }
}
