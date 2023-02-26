package br.unb.cic.tip

object CFGDrawer {

  type pair = (Label, Label)

  def pairs(cfg: CFG): List[pair] = {
    var p = List[pair]()
    cfg.map(n => {
      n.InputEdges.map(e => {
        p = p :+ (e, n.id)
      })
    })
    p
  }

  def show(cfg: CFG): Unit = {
    pairs(cfg).map(p => println(s"${p._1} -> ${p._2}"))
  }
}