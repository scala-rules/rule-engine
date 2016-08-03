package org.scalarules.dsl.nl.grammar

import org.scalarules.dsl.nl.grammar.ConditionsBerekeningGlossary._

class ConditionsBerekening extends Berekening (
  Gegeven(altijd) Bereken
    outputAlwaysAvailable is BigDecimal(10)
  ,
  Gegeven(availableInput is aanwezig) Bereken
    outputShouldBeAvailableIfInputIsAvailable is BigDecimal(11)
  ,
  Gegeven(unavailableInput is afwezig) Bereken
    outputShouldBeAvailableIfInputIsNotAvailable is BigDecimal(12)
)
