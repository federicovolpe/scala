import scala.util.parsing.combinator._

object Interprete extends JavaTokenParsers{
    def interpreta : Parser[(String,List[String])] =>
        funzione  ^^  f 
        //interpreto i files e passo tutto all'esecutore

    }

object esame {
    def main(args: Array[String]) : Unit= {
        args.foreach{arg => val testo = io.Source.fromFile(arg).mkString
            Interprete.parseAll(interpreta, testo) match{
                case Interprete.Success(t,_) => println("successoooo")
                case _ => println("fallimento nooooooo!")
            }
        }
    }
}
