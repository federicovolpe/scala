class Funzioni{
    def squared_numbers (l :List[Any]): List[Int] = { //ritorna la lista di soli numeri quadrati
        l.filter(x => x.isInstanceOf[Int]).map(x => x.asInstanceOf[Int])
        .map(x => x*x)
    }
    val intersect = (l1 :List[Int], l2: List[Int]) => { l1.filter(x1 => l2.contains(x1))}
    
    val symmetric_difference = (l1 :List[Int], l2: List[Int]) =>{
        l1.filter(x1 => !l2.contains(x1)).concat(l2.filter(x2 => !l1.contains(x2)))
    }
}
object secondo extends App{
    val f = new Funzioni
    val list = List(1,2,"ciao",true,5,5.30,"io bne",9)
    println("sto iniziandoooo")
    f.squared_numbers(list).foreach(x => print(s"$x "))
    println()

    val l1 = List(1,2,3,4,5)
    val l2 = List(4,5,6,7,8)
    println("intersect "+ f.intersect(l1,l2))

    println("symmetric difference " + f.symmetric_difference(l1,l2))
}
