package org.scalarules.finance.en

import org.scalarules.finance.core.AbstractTerm

/** Possible terms to use with Per. */
sealed trait Term extends AbstractTerm

// scalastyle:off magic.number

/** Represents a term of 1 month. */
class Month private[en]() extends Period(1) with Term {
  override def toString = "month"
}

/** Represents a term of 3 months. */
class Quarter private[en]() extends Period(3) with Term {
  override def toString = "quarter"
}

/** Represents a term of 6 months. */
class HalfYear private[en]() extends Period(6) with Term {
  override def toString = "half year"
}

/** Representa term of 12 months / 1 year. */
class Year private[en]() extends Period(12) with Term {
  override def toString = "year"
}

// scalastyle:on magic.number

object Term {
  /*
   * These instances must be val instead of object. If they're objects, their type will become 'Year.type'
   * instead of 'Year', and then Per needs to become covariant, which we don't want.
   */

  /** Singleton Term-instance of 1 month. */
  val Month = new Month
  /** Singleton Term-instance of 3 month. */
  val Quarter = new Quarter
  /** Singleton Term-instance of 6 months. */
  val HalfYear = new HalfYear
  /** Singleton Term-instance of 12 months / 1 year. */
  val Year = new Year
}
