package org.scalarules.finance.core

import scala.math.BigDecimal.RoundingMode._

// scalastyle:off method.name

abstract class AbstractPercentage[T <: AbstractPercentage[T]](protected val percentage: BigDecimal) {

  protected def newPercentage(newPercentage: BigDecimal): T

  /** The percentage as a fraction between 0 and 1. */
  protected val internalAsFraction: BigDecimal = percentage / 100

  /** Returns the sum of this percentage and p, as a Percentage. */
  def +[A <: AbstractPercentage[A]](p: A): T = newPercentage(percentage + p.percentage)

  /** Returns the difference between this percentage and p, as a Percentage. */
  def -[A <: AbstractPercentage[A]](p: A): T = newPercentage(percentage - p.percentage)

  /** Returns the percentage of p, as a BigDecimal factor. */
  def *[A <: AbstractPercentage[A]](p: A): BigDecimal = internalAsFraction * p.internalAsFraction

  /** Returns the percentage of n, as type T. */
  def *[A : Quantity](n: A): A = implicitly[Quantity[A]].multiply(n, internalAsFraction)

  /** Returns the percentage of n, as a BigDecimal factor to prevent loss of precision. */
  def *[A <: AbstractPercentage[A]](n: Int): BigDecimal = internalAsFraction * n

  /** Returns the quotient of this percentage and p, as a BigDecimal factor. */
  def /[A <: AbstractPercentage[A]](p: A): BigDecimal = internalAsFraction / p.internalAsFraction

  /** Returns the quotient of this percentage and n, as a BigDecimal factor. */
  def /[A <: AbstractPercentage[A]](n: BigDecimal): BigDecimal = internalAsFraction / n

  protected def internalRoundTo (decimalPlaces: Integer, roundingMode: RoundingMode): T =
    newPercentage(percentage.setScale(decimalPlaces, roundingMode))


}
