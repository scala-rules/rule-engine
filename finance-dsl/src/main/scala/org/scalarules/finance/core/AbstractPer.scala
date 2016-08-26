package org.scalarules.finance.core

import scala.language.higherKinds

// scalastyle:off method.name

abstract class AbstractPer[W, T <: TermType, TermType <: AbstractTerm, PeriodType <: AbstractPeriod[PeriodType]](protected val internalValue: W, protected val internalTerm: T) { // scalastyle:ignore

  type ActualPerConstructor[A, B <: TermType] <: AbstractPer[A, B, TermType, PeriodType]

  protected def newPer[NT <: TermType](newValue: W, newTerm: NT) : ActualPerConstructor[W, NT]
  protected def typedSelf: W ActualPerConstructor T

  /** Returnt de som van deze W en n; per T. */
  def + (n: W ActualPerConstructor T)(implicit ev: Quantity[W]): W ActualPerConstructor T = applySafely(ev.plus, typedSelf, n)

  /** Returnt het verschil tussen deze W en n; per T. */
  def - (n: W ActualPerConstructor T)(implicit ev: Quantity[W]): W ActualPerConstructor T = applySafely(ev.minus, typedSelf, n)

  /** Returnt het product van deze W en n; per T. */
  def * (n: BigDecimal)(implicit ev: Quantity[W]): W ActualPerConstructor T = newPer(ev.multiply(internalValue, n), internalTerm)

  /** Returnt het quotiënt van deze W en n; per T. */
  def / (n: BigDecimal)(implicit ev: Quantity[W]): W ActualPerConstructor T = newPer(ev.divide(internalValue, n), internalTerm)

  /** Converteert dit naar de equivalente waarde per maand. */
  protected def internalMonthly[NT <: TermType](newTerm: NT)(implicit ev: Quantity[W]): W ActualPerConstructor NT = newPer(ev.divide(internalValue, internalTerm.internalMonths), newTerm)

  /** Converteert dit naar de equivalente waarde per jaar. */
  protected def internalYearly[NT <: TermType](newTerm: NT)(implicit ev: Quantity[W]): W ActualPerConstructor NT = newPer(ev.multiply(internalValue, internalTerm.internalFrequencyPerYear), newTerm)


  private def applySafely(f: (W, W) => W, x: W ActualPerConstructor T, y: W ActualPerConstructor T): W ActualPerConstructor T = {
    require(x.internalTerm == y.internalTerm)
    newPer(f(x.internalValue, y.internalValue), x.internalTerm)
  }

}
