import scala.util.parsing.combinator._                                                                                                                                
                                                                                                                                                                      
sealed trait Comando                                                                                                                                                  
//i path in questo caso sono composti da un singolo nome quindi string, altrimenti devono essere cambiati in List[String]                                             
case class Ls            () extends Comando                                                                                                                           
case class Cd   (p: String) extends Comando                                                                                                                           
case class Touch(p: String) extends Comando                                                                                                                           
case class Mkdir(p: String) extends Comando                                                                                                                           
                                                                                                                                                                      
object TParser extends JavaTokenParsers{                                                                                                                              
    def parse: Parser[List[Comando]] = rep1(comando) 
                                                                                                                                                                                      
    //parser per un comando                                                                                                                                                           
    def comando : Parser[Comando] = {                             
        ("touch" | "cd" | "mkdir") ~ ident ^^{                                                                                                                                                                             
            case c ~ p => c match{                                                                                                                                                        
                                case "touch" =>                                                                                                                                           
                                    println("riconosciuto il comando touch " + p)                                                                                                         
                                    Touch(p)                                                                                                                                         
                                case "cd"    =>                                                                                                                                           
                                    println("riconosciuto il comando cd " + p)                                                                                                            
                                    Cd(p)                                                                                                                                                 
                                case "mkdir" =>                                                                                                                                           
                                    println("riconosciuto il comando mkdir " + p)                                                                                                                
                                    Mkdir(p)                                                                                                                                                     
                                }                                                                                                                                                                 
            case _ => throw new java.lang.IllegalArgumentException("errore nel parser comando")                                                                                                   
        }    |                                                       
        "ls" ^^ {case c => println("riconosciuto il comando ls ")                                                                                                            
                                    Ls()                            
                   }                                                
    }                                                                                                                                                                          
                                                                                                                                                                                              
    //riconoscitore per il path                                                                                                                                                               
    /*def path : Parser[List[String]] = (rep1("/" ~> stringLiteral) | stringLiteral) ^^ {                                                                                                     
    def path : Parser[String] = (rep1("/" ~> stringLiteral) | stringLiteral) ^^ {                                                                                                             
        case path if path.isInstanceOf[List[String]] =>                                                                                                                                       
            println("riconosciuto il path: " + path.asInstanceOf[List[String]]) ;path                                                                                                         
        case path if path.isInstanceOf[String] =>                                                                                                                                             
            println("riconosciuto il path: " + path.asInstanceOf[String]) ;path                                                                                                               
        case _ => throw new java.lang.IllegalArgumentException("qualcosa è adato storto nel path")                                                                                                                                                                                
    } */                                                                                                                                                                                      
                                                                                                                                                                                              
}                                                                                                                                                                                             
                                                                                                                                                                                              
object terminale {                                                                                                                                                                            
    def main (args: Array[String]): Unit ={                                                                                                                                                   
        args.foreach{file =>                                                                                                                                                                  
            var testo = io.Source.fromFile(file).mkString                
            println("testo trovato : " + testo)                                                                                                                                               
            TParser.parseAll(TParser.parse, testo) match{                                                                                                                                     
                case TParser.Success(risultato, _) => println("risultato parsato con successo: " + risultato)                                                                                 
                                                                                                                                                                                              
                case TParser.NoSuccess(msg, _) => println("FALLIMENTO: "+ msg)                                                                                                                
            }                                                                                                                                                                                 
        }                                                                                                                                                                                    
    }                                                                                                                                                                                        
}                                                                                                                                                                                            