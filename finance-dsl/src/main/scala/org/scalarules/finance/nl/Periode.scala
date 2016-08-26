package org.scalarules.finance.nl

import org.scalarules.finance.core.AbstractPeriod

// scalastyle:off method.name

/**
 * Representeert een periode in maanden.
 */
case class Periode private[finance](inMaanden: Int) extends AbstractPeriod[Periode](inMaanden) {
  require (inMaanden >= 0)

  override protected def newPeriod(newInMonths: Int): Periode = Periode(newInMonths)

  /** Returnt hoe vaak deze periode in een jaar past. */
  def frequentiePerJaar: Int = internalFrequencyPerYear

  /** Returnt deze periode als een Int, afgekapt op hele jaren. */
  def inAfgekapteJaren: Int = internalInTruncatedYears

  /** Kapt deze periode af (naar beneden) op hele jaren. */
  def afgekaptOpJaren: Periode = internalTruncateToYears

  /** Past f toe op alle jaren binnen deze `Periode`, beginnend bij 0, en afgekapt op hele jaren. */
  def mapOverJaren[T](f: Int => T): Seq[T] = internalMapOverYears(f)

  override def toString = s"$inMaanden maanden"
}

private[nl] trait PeriodeImplicits {
  implicit class IntToTijdsduur(value: Int) {
    /** Maakt een tijdsduur in maanden. */
    def maand: Periode = maanden
    /** Maakt een tijdsduur in maanden. */
    def maanden: Periode = Periode(value)

    /** Maakt een tijdsduur in jaren. */
    def jaar: Periode = Periode(value * 12)
  }

  implicit object OrderingPeriode extends Ordering[Periode] {
    override def compare(x: Periode, y: Periode): Int = x.inMaanden compare y.inMaanden
  }
}

