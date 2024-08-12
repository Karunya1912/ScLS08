object Que2 {

  val isMulThree: Int => Boolean = x => x % 3 == 0

  val isMulFive: Int => Boolean = x => x % 5 == 0

  val isMulBoth: Int => Boolean = x => x % 3 == 0 && x % 5 == 0
  
  def number(num: Int): Unit = {
    num match {
      case x if isMulBoth(x) => println("Multiple of both three and five")
      case x if isMulThree(x) => println("Multiple of three")
      case x if isMulFive(x) => println("Multiple of five")
      case _ => println("Not a multiple of three or five")
    }
  }

  def main(args: Array[String]): Unit = {
    println("Please enter a number:")
    val num = scala.io.StdIn.readLine().toInt
    number(num)
  }
}
