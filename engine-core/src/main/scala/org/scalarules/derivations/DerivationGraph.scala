package org.scalarules.derivations

trait DerivationGraph {

}

case class Node(derivation: Derivation, children: List[Node]) extends DerivationGraph

