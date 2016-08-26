package org.scalarules.finance.en

trait FinanceDsl extends AmountImplicits
  with PeriodImplicits
  with PercentageImplicits
  with PerImplicits
  with Ordering.ExtraImplicits {

  /** Singleton Term-instance of 1 month. */
  val Month = Term.Month
  /** Singleton Termijn-instance of 3 months. */
  val Quarter = Term.Quarter
  /** Singleton Termijn-instance of 6 months. */
  val HalfYear = Term.HalfYear
  /** Singleton Termijn-instance of 12 months / 1 year. */
  val Year = Term.Year
}

