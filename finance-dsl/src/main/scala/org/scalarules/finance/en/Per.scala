package org.scalarules.finance.en

import org.scalarules.finance.core.{AbstractPer, AbstractTerm, Quantity}

// scalastyle:off method.name

/**
  * Indicates a periodic recurrence T of the value W.
  *
  * @param value the recurring value
  * @param term the period after which the value recurs
  * @tparam W type of the value
  * @tparam T type of the term in which the value recurs
  */
case class Per[W, T <: Term](value: W, term: T) extends AbstractPer[W, T, Term, Period](value, term) {

  override type ActualPerConstructor[A, B <: Term] = Per[A, B]

  override protected def newPer[NT <: Term](newValue: W, newTerm: NT): W Per NT = Per(newValue, newTerm)
  override protected def typedSelf: Per[W, T] = this

  /** Converts this to the equivalent value per month. */
  def monthly(implicit ev: Quantity[W]): W Per Month = internalMonthly(Month)

  /** Converts this to the equivalent value per year. */
  def yearly(implicit ev: Quantity[W]): W Per Year = internalYearly(Year)

  /** Applies f to the value and returns the result, per term. */
  def map[V](f: W => V): V Per T = Per(f(value), term)

  /** Applies f to the value and returns the result. */
  def flatMap[V](f: W => V Per T): V Per T = f(value)

  override def toString = s"$value per $term"

}

trait PerImplicits {
  sealed abstract class PerTerm[W](value: W) {
    /** Changes the single value into a value recurring per term. */
    def per[T <: Term](term: T): W Per T = Per(value, term)
  }
  implicit class AmountPerTerm(value: Amount) extends PerTerm[Amount](value)
  implicit class PercentagePerTerm(value: Percentage) extends PerTerm[Percentage](value)
  implicit class BigDecimalPerTerm(value: BigDecimal) extends PerTerm[BigDecimal](value)
  implicit class IntToBigDecimalPerTerm(value: Int) extends PerTerm[BigDecimal](value)
  implicit class StringPerTerm(value: String) extends PerTerm[String](value)


  /**
    * Ensures the inclusion of Per in the scala.Numeric and scala.Ordering type classes for
    * values for which scala.Numeric is defined.
   */
  private def numericPerPeriod[W : Numeric, T <: Term](termijn: T) = new Numeric[W Per T] {
    val ev = implicitly[Numeric[W]]
    override def plus(x: W Per T, y: W Per T): W Per T = Per(ev.plus(x.value, y.value), termijn)
    override def minus(x: W Per T, y: W Per T): W Per T = Per(ev.minus(x.value, y.value), termijn)
    override def times(x: W Per T, y: W Per T): W Per T =
      throw new IllegalStateException("Multiplication of per*per yields per^2, which has no sensible meaning.")
    override def negate(x: W Per T): W Per T = Per(ev.negate(x.value), termijn)
    override def fromInt(x: Int): W Per T = Per(ev.fromInt(x), termijn)
    override def toInt(x: W Per T): Int = ev.toInt(x.value)
    override def toLong(x: W Per T): Long = ev.toLong(x.value)
    override def toFloat(x: W Per T): Float = ev.toFloat(x.value)
    override def toDouble(x: W Per T): Double = ev.toDouble(x.value)
    override def compare(x: W Per T, y: W Per T): Int = ev.compare(x.value, y.value)
  }
  implicit def numericPerMonth[W : Numeric]: Numeric[W Per Month] = numericPerPeriod(Month)
  implicit def numericPerQuarter[W : Numeric]: Numeric[W Per Quarter] = numericPerPeriod(Quarter)
  implicit def numericPerHalfYear[W : Numeric]: Numeric[W Per HalfYear] = numericPerPeriod(HalfYear)
  implicit def numericPerYear[W : Numeric]: Numeric[W Per Year] = numericPerPeriod(Year)

  /**
   * Ensures the inclusion of Per in the Numeric and Ordering type classes
    * for values for which Numeric is defined, but the Term is unspecified.
   */
  implicit def numericPerTerm[W : Quantity : Numeric]: Numeric[W Per Term] = new Numeric[W Per Term] {
    val ev = implicitly[Numeric[W]]
    override def plus(x: W Per Term, y: W Per Term): W Per Term = x + y
    override def minus(x: W Per Term, y: W Per Term): W Per Term = x - y
    override def times(x: W Per Term, y: W Per Term): W Per Term =
      throw new IllegalStateException("Multiplication of per*per yields per^2, which has no sensible meaning.")
    override def negate(x: W Per Term): W Per Term = Per(ev.negate(x.value), x.term)
    override def fromInt(x: Int): W Per Term =
      throw new IllegalStateException("Unable to construct a Per without a valid Term.")
    override def toInt(x: W Per Term): Int = ev.toInt(x.value)
    override def toLong(x: W Per Term): Long = ev.toLong(x.value)
    override def toDouble(x: W Per Term): Double = ev.toDouble(x.value)
    override def toFloat(x: W Per Term): Float = ev.toFloat(x.value)
    override def compare(x: W Per Term, y: W Per Term): Int = {
      require(x.term == y.term)
      ev.compare(x.value, y.value)
    }
  }
}
