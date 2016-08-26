package org.scalarules.finance.en

import java.text.NumberFormat
import java.util.Locale

import org.scalarules.finance.core.{AbstractAmount, Quantity}
import org.scalarules.finance.en

import scala.math.BigDecimal.RoundingMode.RoundingMode

// scalastyle:off method.name

/**
 * Representeert een bedrag in euro's.
 */
case class Amount private[finance](override val value: BigDecimal) extends AbstractAmount[Amount](value) {

  override protected def newAmount(newValue: BigDecimal): Amount = Amount(newValue)

  /** Truncates this amount to the closest hundred. */
  def truncateTo100Euros: Amount = internalTruncateTo100Euros

  /** Truncates this amount to a whole amount (discarding cents). */
  def truncateToEuros: Amount = internalTruncateToEuros

  /** Truncates this amount to two decimal places. */
  def truncateToCents: Amount = internalTruncateToCents

  /** Rounds this amount to two decimal places, using BigDecimal.RoundingMode.HALF_EVEN. */
  def roundToCents: Amount = internalRoundToCents

  def roundTo(decimalPlaces: Int, roundingMode: RoundingMode): Amount = internalRoundTo(decimalPlaces, roundingMode)

  override def toString = NumberFormat.getCurrencyInstance(Amount.netherlands).format(value)
}

object Amount {
  private val netherlands = new Locale("nl", "NL")

  implicit object QuantityAmount extends Quantity[Amount] {
    override def plus(n: Amount, m: Amount) = n + m
    override def minus(n: Amount, m: Amount) = n - m
    override def multiply(n: Amount, m: BigDecimal) = n * m
    override def divide(n: Amount, m: BigDecimal) = n / m
    override def divideAsFraction(n: Amount, m: Amount) = n / m
    override def negate(n: Amount) = n * -1
    override def zero = 0.euros
    override def one = 1.euros
  }
}

trait AmountImplicits {
  abstract class ToAmount(value: BigDecimal) {
    /** Constructs an Amount. */
    def euros: Amount = Amount(value)

    /** Returns the product of this BigDecimal and Amount `b`. */
    def *(b: Amount): Amount = b * value
  }
  implicit class BigDecimalToAmount(value: BigDecimal) extends ToAmount(value)
  implicit class IntToAmount(value: Int) extends ToAmount(value)

  /** It's not possible to multiply a String with an amount, because this would conflict
    * with the `*` method already present on the String-class.
    */
  implicit class StringToAmount(value: String){
    /** Constructs an Amount. */
    def euro: Amount = Amount(BigDecimal(value))
  }


  /** Allows instances of the Amount class to be treated as members of the
    * scala.Numeric type class. */
  implicit object NumericAmount extends Numeric[Amount] {
    override def plus(x: Amount, y: Amount): Amount = x + y
    override def minus(x: Amount, y: Amount): Amount = x - y
    override def times(x: Amount, y: Amount): Amount =
      throw new UnsupportedOperationException("Multiplication of `amount * amount` would yield an amount^2, which has no sensible meaning.")
    override def negate(x: Amount): Amount = Amount(-x.value)
    override def fromInt(x: Int): Amount = x.euros
    override def toInt(x: Amount): Int = throw new UnsupportedOperationException("toInt would cause loss of precision.")
    override def toLong(x: Amount): Long = throw new UnsupportedOperationException("toLong would cause loss of precision.")
    override def toFloat(x: Amount): Float = throw new UnsupportedOperationException("toFloat would cause loss of precision.")
    override def toDouble(x: Amount): Double = throw new UnsupportedOperationException("toDouble would cause loss of precision.")
    override def compare(x: Amount, y: Amount): Int = x.value compare y.value
  }

//  implicit object QuantityAmount extends Quantity[Amount] {
//    override def plus(n: Amount, m: Amount) = n + m
//    override def minus(n: Amount, m: Amount) = n - m
//    override def multiply(n: Amount, m: BigDecimal) = n * m
//    override def divide(n: Amount, m: BigDecimal) = n / m
//    override def divideAsFraction(n: Amount, m: Amount) = n / m
//    override def negate(n: Amount) = n * -1
//    override def zero = 0.euros
//    override def one = 1.euros
//  }

}
