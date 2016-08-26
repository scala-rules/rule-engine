package org.scalarules.finance.en

import org.scalarules.finance.core.AbstractPeriod

// scalastyle:off method.name

/**
 * Represents a period of time in months.
 */
case class Period private[finance](override val inMonths: Int) extends AbstractPeriod[Period](inMonths) {
  require (inMonths >= 0)

  override protected def newPeriod(newInMonths: Int): Period = Period(newInMonths)

  /** Returns how many times this period fits within a year. */
  def frequencyPerYear: Int = internalFrequencyPerYear

  /** Returns this period as an Int, truncated to full years. */
  def inTrucatedYears: Int = internalInTruncatedYears

  /** Truncates this period down to whole years. */
  def truncateToYears: Period = internalTruncateToYears

  /** Applies f to all whole years within this Period, starting at 0. */
  def mapOverYears[T](f: Int => T): Seq[T] = internalMapOverYears(f)

  override def toString = s"$inMonths months"

}

private[en] trait PeriodImplicits {
  implicit class IntToPeriod(value: Int) {
    /** Constructs a period in months. */
    def month: Period = months
    /** Constructs a period in months. */
    def months: Period = Period(value)

    /** Constructs a period in years. */
    def year: Period = Period(value * 12)
  }

  implicit object OrderingPeriod extends Ordering[Period] {
    override def compare(x: Period, y: Period): Int = x.inMonths compare y.inMonths
  }
}
