package org.scalarules.finance.core

import scala.math.BigDecimal.RoundingMode._

// scalastyle:off method.name

abstract class AbstractAmount[T <: AbstractAmount[T]](protected val value: BigDecimal) {

  protected def newAmount(newValue: BigDecimal): T

  /** Returns the sum of this amount and n. */
  def +[A <: AbstractAmount[A]](n: A): T = newAmount(value + n.value)

  /** Returns the difference between this amount and n. */
  def -[A <: AbstractAmount[A]](n: A): T = newAmount(value - n.value)

//  /** Returns the percentage of n, as type T. */
//  def *[A : Quantity](n: A): A = implicitly[Quantity[A]].multiply(n, internalAsFraction)

  /** Returns the product of this amount and n. */
  def *(n: BigDecimal): T = newAmount(value * n)

  /** Returns the quotient of this amount and n. */
  def /(n: BigDecimal): T = newAmount(value / n)

  /** Returns the quotient of this amount and n. */
  def /[A <: AbstractAmount[A]](n: A): BigDecimal = value / n.value

  protected def internalRoundTo(decimalPlaces: Int, roundingMode: RoundingMode): T = newAmount(value.setScale(decimalPlaces, roundingMode))

  /** Truncates this amount (down) to the specified number of decimal places. */
  protected def truncateTo(decimalPlaces: Int): T = internalRoundTo(decimalPlaces, BigDecimal.RoundingMode.FLOOR)

  /** Truncates this amount to the closest hundred. */
  protected def internalTruncateTo100Euros: T = truncateTo(-2) // scalastyle:ignore magic.number

  /** Truncates this amount to a whole amount (discarding cents). */
  protected def internalTruncateToEuros: T = truncateTo(0)

  /** Truncates this amount to two decimal places. */
  protected def internalTruncateToCents: T = truncateTo(2)

  /** Rounds this amount to two decimal places, using BigDecimal.RoundingMode.HALF_EVEN. */
  protected def internalRoundToCents: T = internalRoundTo(2, BigDecimal.RoundingMode.HALF_EVEN)


}
