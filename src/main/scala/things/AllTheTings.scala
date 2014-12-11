package things


/**
 * Created by scott on 12/9/14.
 */
object AllTheTings {

  case class Example(description: String, code: () => Unit) {
    def run = {
      println(
        s"""=========================================================================================
          |Running Example: $description
          |=========================================================================================
        """.stripMargin)
      code()
    }
  }

  def example(description: String) =  new {
    def code(c: => Unit) = Example(description, c _)
  }

  implicit class StringToExample(val description: String) {
    def code(block: => Unit) = {
      Example(description, block _)
    }
  }

}



object PatternMatchingThings extends App {

  import AllTheTings._

  "Pattern Matching support disjunctions (or)".code {

    val i = 0;
    val d = 0.0
    val s = "Thingz"

    val mtch = (a: Any ) => a match {
      case v : String              => s"$v is a String!"
      case _: Int |  _: Double     => s"$a is a number!"
    }

    println(mtch(s))
    println(mtch(i))
    println(mtch(d))

  }.run


}
