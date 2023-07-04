object prova extends App {
    class Person (name : String) {
        def apply(age :Int) = println(s"i have age $age years") 
    }
    List("India","USA","China","Japan")
    .map(x => assegna(x))
    divisors(18).foreach{print}

    def assegna(s : String) = {
        s match {
            case "India" => println("Dheli")
            case "USA" => println("Washington D.C.")
            case "Japan" => println("Tokyo")
            case _ => println("i dont know")
        }
    }
    def divisors(n: Int, divisore : Int = 2, res : List[Int] = Nil): List[Int] = {
        if(divisore > n){
            res
        }else if(n % divisore == 0){
            println(s"$divisore divisore")
            divisors(n, divisore+1, res:+divisore)
        }else{
            divisors(n, divisore+1, res)
        }
    }
    /*
    obiettivo:
    ritornare tutte le coppie di due liste*/

}