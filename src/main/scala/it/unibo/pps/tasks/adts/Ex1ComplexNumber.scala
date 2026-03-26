package it.unibo.pps.tasks.adts

import it.unibo.pps.u04.adts.SequenceADT.*

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex

    def complex(re: Double, im: Double): Complex

    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    import it.unibo.pps.u04.adts.SequenceADT.*

    case class ComplexImpl(reVal: Double, imVal: Double)

    // Change assignment below: should probably define a case class and use it?
    opaque type Complex = ComplexImpl

    def complex(re: Double, im: Double): Complex =
      ComplexImpl(re, im)

    extension (complex: Complex)
      def re(): Double = complex.reVal
      def im(): Double = complex.imVal
      def sum(other: Complex): Complex = ComplexImpl(complex.reVal + other.reVal, complex.imVal + other.imVal)
      def subtract(other: Complex): Complex = ComplexImpl(complex.reVal - other.reVal, complex.imVal - other.imVal)
      def asString(): String =
        val r = complex.reVal
        val i = complex.imVal
        (r, i) match
          case (re, 0) => s"$re"
          case (0, im) => s"${im}i"
          case (re, im) =>
            val sign = if im > 0 then "+" else "-"
            s"$re $sign ${math.abs(im)}i"
