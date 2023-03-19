package br.unb.cic.tip

import br.unb.cic.tip.*
import br.unb.cic.tip.Stmt.*
import br.unb.cic.tip.Expression.*
import br.unb.cic.tip.Node.*

type Path = List[Stmt]

//def createPath(from: Stmt, to: Stmt, cfg: Graph): Path = to match {
//  case from => List()
//  case _ => List(from) //::: successors(from, cfg).toList.map(s => createPath(s, to, cfg))
//}
def createPath(from: Stmt, to: Stmt, cfg: Graph): Path = {
  if (to == from) {
    List()
  } else {
    List(from) ++ successors(from, cfg).toList.map(s => createPath(s, to, cfg)).foldLeft(List())(_ ++ _)
  }
}