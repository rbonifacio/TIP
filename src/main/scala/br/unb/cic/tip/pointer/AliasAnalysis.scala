package br.unb.cic.tip.pointer

import br.unb.cic.tip.utils.{BasicExp, Stmt, VariableExp}

type ResultAlias = Boolean

object AliasAnalysis {

  def run(body: Stmt, var1: BasicExp, var2: BasicExp): ResultAlias = {
    val PT = BasicAndersen.pointTo(body)
    (PT(var1) intersect PT(var2)) != Set()
  }
}
