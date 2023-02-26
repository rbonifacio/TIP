package br.unb.cic.tip

object CFGDrawer {

  type pair = (Label, Label)

  def pairs(cfg: CFG): Set[pair] = {
    var p = Set[pair]()
    cfg.map(n => {
      n.inputEdges.map(e => {
        p += (e, n.id)
      })
      n.outputEdges.map(e => {
        p += (n.id, e)
      })
    })
    p
  }

  def show(cfg: CFG): Unit = {
    pairs(cfg).map(p => println(s"${p._1} -> ${p._2}"))
  }
}