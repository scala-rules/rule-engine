package org.scalarules.finance.core

// scalastyle:off method.name

private[core] trait PeriodField {
  /** The number of months of which the implementing Period consists. */
  def internalMonths: Int
  /** Returns how many times this period fits within a year. */
  def internalFrequencyPerYear: Int = 12 / internalMonths
}

abstract class AbstractPeriod[+T <: AbstractPeriod[T]](private[finance] val inMonths: Int) extends PeriodField {
  require (inMonths >= 0)

  override val internalMonths: Int = inMonths

  protected def newPeriod(newInMonths: Int): T

  /** Returns the sum of this period and n. */
  def +[A <: AbstractPeriod[A]](n: A): T = newPeriod(inMonths + n.inMonths)

  /** Returns the difference between this period and n. */
  def -[A <: AbstractPeriod[A]](n: A): T = newPeriod(inMonths - n.inMonths)

  /** Returns this period as an Int, truncated to full years. */
  private[finance] def internalInTruncatedYears: Int = inMonths / 12

  /** Truncates this period down to whole years. */
  private[finance] def internalTruncateToYears: T = newPeriod(internalInTruncatedYears * 12)

  /** Applies f to all whole years within this [[AbstractPeriod]], starting at 0. */
  private[finance] def internalMapOverYears[A](f: Int => A): Seq[A] = (0 until internalInTruncatedYears) map f

}

