
/* Copyright 2009-2021 EPFL, Lausanne */

package stainless
package verification

class ReachabilityProbeInjector(override val s: extraction.Trees,
                     override val t: extraction.Trees)
                    (using override val context: inox.Context)
  extends transformers.Transformer with extraction.SimpleFunctions with extraction.IdentitySorts { self =>

  import s._
  import exprOps._

  override protected type TransformerContext = s.Symbols
  override def getContext(symbols: s.Symbols): TransformerContext = symbols

  private[this] class Identity(override val s: self.s.type, override val t: self.t.type) extends transformers.ConcreteTreeTransformer(s, t)
  private[this] val identity = new Identity(self.s, self.t)

  override protected type FunctionSummary = Unit

  override def extractFunction(symbols: TransformerContext, fd: s.FunDef): (t.FunDef, FunctionSummary) = {
    val specced = BodyWithSpecs(fd.fullBody)

    def transform(e: s.Expr): s.Expr = e match {
      case ie @ s.IfExpr(c, t, e) =>
        s.IfExpr(transform(c), s.ReachabilityProbe(transform(t)), s.ReachabilityProbe(transform(e))).copiedFrom(ie)

      case me @ s.MatchExpr(scrut, cases) =>
        s.MatchExpr(scrut, cases.map {
          cse => if cse.optGuard.isEmpty  then {
            cse.copy(rhs = s.ReachabilityProbe(transform(cse.rhs))).copiedFrom(cse)
          }
          else {
            cse.copy(optGuard = Option(transform(cse.optGuard.get)), rhs = s.ReachabilityProbe(transform(cse.rhs))).copiedFrom(cse)
            
          }
        }).copiedFrom(me)

      case _ => super.transform(e)
    }

    val newSpecced = specced.copy(body = transform(specced.body))
    (identity.transform(fd.copy(fullBody = newSpecced.reconstructed).setPos(fd)), new FunctionSummary)
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
