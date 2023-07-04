/*import scala.util.parsing.combinator._

object Interprete extends JavaTokenParsers{
    def interpreta: Unit = {
        //interpreta le varie parti e le stampa
    }

}

object primo {
    def main(args: Array[String]): Unit = {
        //leggo dal file specificato negli args
        val file = args[1]
        val testo = io.Source.fromFile(file).mkString
        Interprete.parseAll(Interprete.interpreta, testo) match {
            case Interprete.Success(t,_) => 
                println(s"il risultato è : $t")
            case e =>
                println("Errore qualcosa è andato storto nel parsing")
        }
    }
}*/