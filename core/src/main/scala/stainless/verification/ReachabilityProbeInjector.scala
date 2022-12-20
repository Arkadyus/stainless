
/* Copyright 2009-2021 EPFL, Lausanne */

package stainless
package verification


object ReachabilityInjector extends inox.DebugSection("reachability-injector")

class ReachabilityProbeInjector(override val s: extraction.Trees,
                     override val t: extraction.Trees)
                    (using override val context: inox.Context)
  extends extraction.CachingPhase
    with extraction.NoSummaryPhase
    with extraction.IdentitySorts
    with extraction.SimpleFunctions { self =>

  import t._
  import exprOps._
  import s._

  override protected type TransformerContext = s.Symbols
  override def getContext(symbols: s.Symbols): TransformerContext = symbols

  override protected type FunctionSummary = Unit
  override protected final val funCache = new ExtractionCache[s.FunDef, (FunctionResult, FunctionSummary)]((fd, context) =>
    getDependencyKey(fd.id)(context.symbols)  // TODO fix
  )

  override def extractFunction(symbols: TransformerContext, fd: s.FunDef): (t.FunDef, FunctionSummary) = {
    object transformer extends stainless.transformers.TreeTransformer {
      override val s: self.s.type = self.s
      override val t: self.t.type = self.t

      // As before but should return a t.Expr
      override def transform(e: s.Expr): t.Expr = e match {
        case s.IfExpr(c, th, e) =>
          t.IfExpr(transform(c), t.ReachabilityProbe(transform(th)), t.ReachabilityProbe(transform(e)))
        // case cse @ s.MatchCase(pat, optGuard, rhs) =>
        //   if optGuard.isEmpty then {
        //     t.MatchCase(transform(pat), Option.empty, t.ReachabilityProbe(transform(rhs))).copiedFrom(cse)
        //   } else {
        //     t.MatchCase(transform(pat), Option(transform(optGuard.get)), transform(rhs)).copiedFrom(cse)
        //   }
        case _ => super.transform(e)
      }
    }
    (transformer.transform(fd), ())
  }
}

object ReachabilityProbeInjector {
  def apply(tr: extraction.Trees)(using inox.Context): extraction.ExtractionPipeline {
    val s: tr.type
    val t: tr.type
  } = {
    class Impl(override val s: tr.type, override val t: tr.type) extends ReachabilityProbeInjector(s, t)
    new Impl(tr, tr)
  }
}
