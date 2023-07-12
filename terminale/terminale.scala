import scala.util.parsing.combinator._                                                                                                          
                                                                                                                                                
sealed trait Comando                                                                                                                            
//i path in questo caso sono composti da un singolo nome quindi string, altrimenti devono essere cambiati in List[String]                       
case class Ls   (p: List[String]) extends Comando                                                                                                        
case class Cd   (p: List[String]) extends Comando                                                                                                        
case class Touch(p: List[String]) extends Comando                                                                                                        
case class Mkdir(p: List[String]) extends Comando                                                                                                        
                                                                                                                                                
object TParser extends JavaTokenParsers{                                                                                                        
    def parse: Parser[List[Comando]] = rep1(comando) ^^ {                                                                                      
        case c => List(c)                                                                                                                 
        case _ => throw new java.lang.RuntimeException("qualcosa in parse è andato male")                                                       
    }                                                                                                                                 
                                                                                                                                      
    //parser per un comando                                                                                                           
    def comando : Parser[Comando] = ("touch" | "ls" | "cd" | "mkdir") ~ path ^^{                                                            
        case c ~ p => c match{                                                                                                                  
                            case "touch" => Touch(p)                                                                                                           
                            case "ls"    => Ls(p)                                                                                           
                            case "cd"    => Cd(p)                                                                                           
                            case "mkdir" => Mkdir(p)                                                                                                           
                            }                                                                                                               
        case _ => throw new java.lang.IllegalArgumentException("errore nel parser comando")                                                 
    }                                                                                                                                       
                                                                                                                                            
    //riconoscitore per il path                                                                                                             
    def path : Parser[List[String]] = rep1("/" ~> stringLiteral) ^^{                                                                        
        case p => p                                                                                                                         
        case _ => throw new IllegalArgumentException("il parser del path non ha funzionato a dovere")                                       
    }                                                                                                                                       
                                                                                                                                            
}                                                                                                                                           
                                                                                                                                            
object terminale {                                                                                                                          
    def main (args: Array[String]): Unit ={                                                                                                 
        //args.foreach(file =>                                                                                                              
            //val testo = io.Source.fromFile(file)).mkString                                                                                
            val input = io.StdIn.readLine()                                                                                                 
            TParser.parseAll(TParser.path, input) match{                                                                                    
                case TParser.Success(risultato, _) => println("risultato parsato con successo: " + risultato)                              
                                                                                                                                           
                case e => println("FALLIMENTO") }                                                                                          
    }                                                                                                                                      
}                                                                                                                                          