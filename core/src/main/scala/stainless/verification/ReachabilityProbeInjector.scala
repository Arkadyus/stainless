
/* Copyright 2009-2021 EPFL, Lausanne */

package stainless
package verification

class ReachabilityProbeInjector(override val s: extraction.Trees,
                     override val t: extraction.Trees)
                    (using override val context: inox.Context)
  extends extraction.CachingPhase with extraction.SimpleFunctions with extraction.IdentitySorts{ self =>

  import s._
  import exprOps._

  override protected final val funCache = new ExtractionCache[s.FunDef, (FunctionResult, FunctionSummary)]((fd, symbols) =>
    getDependencyKey(fd.id)(using symbols)
  )

  override protected type TransformerContext = s.Symbols
  override def getContext(symbols: s.Symbols): TransformerContext = symbols

  override protected type FunctionSummary = Unit

  override def extractFunction(symbols: TransformerContext, fd: s.FunDef): (t.FunDef, FunctionSummary) = {
    object transformer extends stainless.transformers.TreeTransformer {
      override val s: self.s.type = self.s
      override val t: self.t.type = self.t

      override def transform(e: s.Expr): t.Expr = e match {
        case ie @ s.IfExpr(c, thenn, e) =>
          t.IfExpr(transform(c), t.ReachabilityProbe(transform(thenn)), t.ReachabilityProbe(transform(e))).copiedFrom(ie)

        case me @ s.MatchExpr(scrut, cases) =>
          t.MatchExpr(transform(scrut), cases.map {
            // We actually dont want to transform pattern but it needs to somehow become of type t instead of s
            cse => if cse.optGuard.isEmpty  then {
              t.MatchCase(super.transform(cse.pattern), Option.empty, t.ReachabilityProbe(transform(cse.rhs))).copiedFrom(cse)
            }
            else {
              t.MatchCase(super.transform(cse.pattern), Option(transform(cse.optGuard.get)), t.ReachabilityProbe(transform(cse.rhs))).copiedFrom(cse)
            }
          }).copiedFrom(me)

        case _ => super.transform(e)
      }
    }

    (transformer.transform(fd), ())
  }

  override protected def combineSummaries(summaries: AllSummaries): extraction.ExtractionSummary = {
    extraction.ExtractionSummary.NoSummary
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
