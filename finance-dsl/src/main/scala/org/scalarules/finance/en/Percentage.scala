package org.scalarules.finance.en

import org.scalarules.finance.core.{AbstractPercentage, Quantity}

import scala.math.BigDecimal.RoundingMode._

// scalastyle:off method.name


/**
 * Represents a percentage.
 *
 * A Percentage must be between 0.0 and 100.0.
 */
case class Percentage private[finance] (override val percentage: BigDecimal) extends AbstractPercentage[Percentage](percentage) with Ordered[Percentage] {

  override protected def newPercentage(newPercentage: BigDecimal): Percentage = Percentage(newPercentage)

  /** The percentage as a fraction between 0 and 1. */
  val asFraction: BigDecimal = internalAsFraction

  def roundTo (decimalPlaces: Integer, roundingMode: RoundingMode): Percentage = internalRoundTo(decimalPlaces, roundingMode)

  override def compare(that: Percentage) = percentage compare that.percentage

  override def toString = s"$percentage%"
}

trait PercentageImplicits {
  /*
   * The 'percent' method in several of the following implicit classes can't be called '%',
   * because most numeric types already have a % method (meaning 'modulo'). The compiler keeps
   * expecting a parameter which isn't there, because it searches the real method before it
   * searches implicits.
   */

  abstract class ToPercentage(value: BigDecimal) {
    /** Constructs a Percentage. */
    def percent: Percentage = Percentage(value)
  }

  implicit class BigDecimalToPercentage(value: BigDecimal) extends ToPercentage(value)
  implicit class IntToPercentage(value: Int) extends ToPercentage(value)
  implicit class StringToPercentage(value: String) extends ToPercentage(BigDecimal(value))

  implicit class IntWithPercentage(value: Int) {
    /** Returns the product of this Int and Percentage n as a BigDecimal. */
    def * (p: Percentage): BigDecimal = p * BigDecimal(value)
  }
  implicit class NumberLikeWithPercentage[T : Quantity](value: T) {
    /** Returns the product of this NumberLike T and Percentage n as T. */
    def * (p: Percentage): T = p * value
  }
}
