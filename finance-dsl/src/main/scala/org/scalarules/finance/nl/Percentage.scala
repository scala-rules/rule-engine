package org.scalarules.finance.nl

import org.scalarules.finance.core.{AbstractPercentage, Quantity}

import scala.math.BigDecimal.RoundingMode.RoundingMode

// scalastyle:off method.name

/**
 * Representeert een percentage.
 */
case class Percentage private[finance] (override val percentage: BigDecimal) extends AbstractPercentage[Percentage](percentage) with Ordered[Percentage] {

  override protected def newPercentage(newPercentage: BigDecimal): Percentage = Percentage(newPercentage)

  /** Het percentage als een fractie tussen 0 en 1. */
  val alsFractie: BigDecimal = internalAsFraction

  def afgerondOp (aantalDecimalen: Integer, afrondingsWijze: RoundingMode): Percentage =
    percentage.setScale(aantalDecimalen, afrondingsWijze).procent

  override def compare(that: Percentage) = percentage compare that.percentage

  override def toString = s"$percentage%"
}

trait PercentageImplicits {
  /*
   * The 'procent' method in several of the following implicit classes can't be called '%',
   * because most numeric types already have a % method (meaning 'modulo'). The compiler keeps
   * expecting a parameter which isn't there, because it searches the real method before it
   * searches implicits.
   */

  abstract class ToPercentage(waarde: BigDecimal) {
    /** Maakt een `Percentage`. */
    def procent: Percentage = Percentage(waarde)
  }

  implicit class BigDecimalToPercentage(waarde: BigDecimal) extends ToPercentage(waarde)
  implicit class IntToPercentage(waarde: Int) extends ToPercentage(waarde)
  implicit class StringToPercentage(waarde: String) extends ToPercentage(BigDecimal(waarde))

  implicit class IntWithPercentage(waarde: Int) {
    /** Returnt het product van deze Int en Percentage n als BigDecimal. */
    def * (p: Percentage): BigDecimal = p * BigDecimal(waarde)
  }
  implicit class QuantityWithPercentage[T : Quantity](waarde: T) {
    /** Returnt het product van deze Quantity T en Percentage n als T. */
    def * (p: Percentage): T = p * waarde
  }
}
