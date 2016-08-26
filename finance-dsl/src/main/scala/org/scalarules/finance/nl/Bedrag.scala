package org.scalarules.finance.nl

import java.text.NumberFormat
import java.util.Locale

import org.scalarules.finance.core.{AbstractAmount, Quantity}

import scala.math.BigDecimal.RoundingMode._
import scala.language.implicitConversions

// scalastyle:off method.name

/**
 * Representeert een bedrag in euro's.
 */
case class Bedrag private[finance] (waarde: BigDecimal) extends AbstractAmount[Bedrag](waarde) {

  override protected def newAmount(newValue: BigDecimal): Bedrag = Bedrag(newValue)

  /** Kapt dit bedrag af (naar beneden) op een rond honderdtal euro's. */
  def afgekaptOp100Euro: Bedrag = internalTruncateTo100Euros

  /** Kapt dit bedrag af (naar beneden) op ronde euro's. */
  def afgekaptOpEuros: Bedrag = internalTruncateToEuros

  /** Kapt dit bedrag af (naar beneden) op hele centen. */
  def afgekaptOpCenten: Bedrag = internalTruncateToCents

  /** Rondt dit bedrag af op hele centen, volgens BigDecimal.RoundingMode.HALF_EVEN. */
  def afgerondOpCenten: Bedrag = internalRoundToCents

  def afgerondOp(aantalDecimalen: Integer, afrondingsWijze: RoundingMode): Bedrag = internalRoundTo(aantalDecimalen, afrondingsWijze)

  override def toString = NumberFormat.getCurrencyInstance(Bedrag.nederland).format(waarde)
}

object Bedrag {
  private val nederland = new Locale("nl", "NL")

  implicit object QuantityBedrag extends Quantity[Bedrag] {
    override def plus(n: Bedrag, m: Bedrag) = n + m
    override def minus(n: Bedrag, m: Bedrag) = n - m
    override def multiply(n: Bedrag, m: BigDecimal) = n * m
    override def divide(n: Bedrag, m: BigDecimal) = n / m
    override def divideAsFraction(n: Bedrag, m: Bedrag) = n / m
    override def negate(n: Bedrag) = n * -1
    override def zero = 0.euro
    override def one = 1.euro
  }
}

trait BedragImplicits {
  abstract class ToBedrag(value: BigDecimal) {
    /** Maakt een Bedrag. */
    def euro: Bedrag = Bedrag(value)

    /** Returnt het product van deze BigDecimal en Bedrag b. */
    def *(b: Bedrag): Bedrag = b * value
  }
  implicit class BigDecimalToBedrag(value: BigDecimal) extends ToBedrag(value)
  implicit class IntToBedrag(value: Int) extends ToBedrag(value)

  private[finance] implicit def convertIntToBedrag(value: Int): Bedrag = Bedrag(BigDecimal(value))

  /** Het is niet mogelijk om een String te vermenigvuldigen met een Bedrag
    * Dit conflicteert met String's eigen * functie en is dus niet geimplementeerd*/
  implicit class StringToBedrag(value: String){
    /** Maakt een Bedrag. */
    def euro: Bedrag = Bedrag(BigDecimal(value))
  }


  /** Zorgt ervoor dat zaken als "sum" gemakkelijk kunnen worden berekend op verzamelingen van Bedrag. */
  implicit object NumericBedrag extends Numeric[Bedrag] {
    override def plus(x: Bedrag, y: Bedrag): Bedrag = x + y
    override def minus(x: Bedrag, y: Bedrag): Bedrag = x - y
    override def times(x: Bedrag, y: Bedrag): Bedrag =
      throw new UnsupportedOperationException("Vermenigvuldiging van bedrag*bedrag zou een bedrag^2 geven, wat niets betekent.")
    override def negate(x: Bedrag): Bedrag = Bedrag(-x.waarde)
    override def fromInt(x: Int): Bedrag = x.euro
    override def toInt(x: Bedrag): Int = throw new UnsupportedOperationException("toInt zou leiden tot een verlies van precisie.")
    override def toLong(x: Bedrag): Long = throw new UnsupportedOperationException("toLong zou leiden tot een verlies van precisie.")
    override def toFloat(x: Bedrag): Float = throw new UnsupportedOperationException("toFloat zou leiden tot een verlies van precisie.")
    override def toDouble(x: Bedrag): Double = throw new UnsupportedOperationException("toDouble zou leiden tot een verlies van precisie.")
    override def compare(x: Bedrag, y: Bedrag): Int = x.waarde compare y.waarde
  }

//  implicit object QuantityBedrag extends Quantity[Bedrag] {
//    override def plus(n: Bedrag, m: Bedrag) = n + m
//    override def minus(n: Bedrag, m: Bedrag) = n - m
//    override def multiply(n: Bedrag, m: BigDecimal) = n * m
//    override def divide(n: Bedrag, m: BigDecimal) = n / m
//    override def divideAsFraction(n: Bedrag, m: Bedrag) = n / m
//    override def negate(n: Bedrag) = n * -1
//    override def zero = 0.euro
//    override def one = 1.euro
//  }

}
