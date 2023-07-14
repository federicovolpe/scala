import scala.util.parsing.combinator._

sealed trait Comando
case class Push(n : Int) extends Comando
case class Pop() extends Comando
case class Find(n : Int) extends Comando

object qParser extends JavaTokenParsers {
  def parse : Parser[List[Comando]] = rep1(comando)
  
  def comando : Parser[Comando] = ("push" | "pop" | "find") ~ opt(wholeNumber) ^^ {
    case "push" ~ Some(n) => Push(n.asInstanceOf[Int])
    case "pop" ~ None => Pop()
    case "find" ~ Some(n) => Find(n.asInstanceOf[Int])
    case _ => throw new IllegalArgumentException("comando non riconosciuto")
  }
}
object queue{
  def main(args :Array[String]): Unit = {
    //reading file 
    args.foreach{file =>
      val testo = io.Source.fromFile(file).mkString
      qParser.parseAll(qParser.parse, testo) match {
        case qParser.Success(parsato, _) => println("risultato parsato: " + parsato)

        case qParser.NoSuccess(msg, _) => println("qualcosa è andato storto nel parsing")
      }
    }
  }
}
