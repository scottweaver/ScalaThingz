package things.FPiS.chapter6

/**
 * Created by scott on 12/11/14.
 */
object Exercises {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def simple(seed: Long): RNG = {
      new RNG {
        def nextInt = {
          val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
          ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
        }
      }
    }

    def reallySimple = simple(System.currentTimeMillis)
  }

  type Rand[+A] = RNG => (A, RNG)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def printlnRNG(i: Any) = println(s"Your pseudo-random number is/are $i.")

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)
}

object Exercise1 extends App {
  import Exercises._

  def positiveInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (i, rng2) if i == Int.MinValue => positiveInt(rng2)
      case (i, rng2) => (i.abs, rng2)
    }
  }

  printlnRNG(positiveInt(RNG.reallySimple)._1)
}

object Exercise2 extends App {
  import Exercises._
  import Exercise1._

  def double(rng: RNG): (Double, RNG) = {
    val (ri, rng2) = positiveInt(rng)
    (ri.toDouble / (Int.MaxValue.toDouble + 1), rng2)
  }

  printlnRNG(double(RNG.reallySimple)._1)
}

object Exercise3 extends App {
  import Exercises._
  import Exercise1._
  import Exercise2._

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (ri, rng2) = positiveInt(rng)
    val (rd, rng3) = double(rng2)
    ((ri, rd), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }


  printlnRNG(intDouble(RNG.reallySimple)._1)
  printlnRNG(doubleInt(RNG.reallySimple)._1)
  printlnRNG(double3(RNG.reallySimple)._1)

}

object Exercise4 extends App {

  import Exercises._

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def run(someRng: RNG, count: Int, rngs: (List[Int], RNG)): (List[Int], RNG) = {
      if (count > 0) {
        val (ri, rng2) = someRng.nextInt
        run(rng2, count - 1, (ri :: rngs._1, rng2))
      }
      else
        rngs
    }

    run(rng, count.abs, (Nil, rng))
  }

  printlnRNG(ints(25)(RNG.reallySimple)._1)
}

object Exercise5 extends App {
  import Exercises._

  val n = 15
  val r = map(_.nextInt){i => (i % n).abs}
  printlnRNG(r(RNG.reallySimple))
}

object Exercise6 extends App {
  import Exercises._
  import Exercise1._

  def elegantDouble(rng: RNG): (Double, RNG) = map(positiveInt){_.toDouble / (Int.MaxValue.toDouble + 1) }(rng)

  printlnRNG(elegantDouble(RNG.reallySimple))
}

object Exercise7 extends App {
  import Exercises._
  import Exercise6._
  import Exercise1._

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def elegantIntDouble(rng: RNG): ((Int, Double), RNG) = {
    map2(positiveInt, elegantDouble)((a, b) => (a, b))(rng)
  }

  printlnRNG(elegantIntDouble(RNG.reallySimple))

}

object Exercise8 extends App {

}

object Exercise9 extends App {
  import Exercises._

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = g(f(RNG.reallySimple)._1)

  def positiveInt: Rand[Int] = {
    flatMap(_.nextInt){ i => if(i != Int.MinValue) {rng => (i.abs, rng)} else positiveInt}
  }

  printlnRNG(positiveInt(RNG.reallySimple))
}

object Exercise10 extends App {
  import Exercises._
  import Exercise9._

  def newMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) { i => { rng => (f(i), rng)} }

  def newMap2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) { a => flatMap(rb) { b => {rngb =>{(f(a, b), rngb)}}}  }

  def elegantDouble(rng: RNG): (Double, RNG) = newMap(positiveInt){_.toDouble / (Int.MaxValue.toDouble + 1) }(rng)

  def elegantIntDouble(rng: RNG): ((Int, Double), RNG) = {
    newMap2(positiveInt, elegantDouble)((a, b) => (a, b))(rng)
  }

  printlnRNG(elegantIntDouble(RNG.reallySimple))

}


object Exercise11 extends App {

  case class State[S, +A](run: S => (A, S))

  def unit[A, S](s: S => (A, S)): State[S, A] = State[S, A](s)

  val sunit = unit[String, Int]{i: Int => (i.toString, i)}

  println(sunit.run(12))


}
