class Monoid(n: Set[Any], f:(Any,Any) => Any, identity: Any){
    val èmonoide = identità && chiusura && associatività
    //verifica che l'elemento identità funzioni f(a,i) = f(i,a) = a
    def identità : Boolean =  { 
        n.foreach(x => if (!(f(x,identity) == (f(identity,x)) && f(identity,x) == (x))) 
        return false)
        return true
    }
    //per due qualsiasi numeri a e b f(a,b) appartiene al set
    def chiusura : Boolean = {
        n.foreach(x => n.foreach (y => if(!(n.contains(f(x,y)))) return false))
        return true
    }
    //associatività : f(f(a,b),c) = f(a,(b,c))
    def associatività : Boolean = {
        n.foreach(a => n.foreach(b => n.foreach(c => if(!(f(f(a,b),c) == f(a,f(b,c)))) return false)))
        return true
    }
}
class Group
class Ring

object es4 extends App{

  val set : Set[Any] = Set(true, false)
  def logicalOr (x:Any,y:Any) : Any = x.asInstanceOf[Boolean] || y.asInstanceOf[Boolean]  
  val id = false
  val monoid = new Monoid(set, logicalOr, id)
  println("identita funziona? " + monoid.identità)
  println("chiusura funziona? " + monoid.chiusura)
  println("associatività funziona? " + monoid.associatività)
  println("overall : "+ monoid.èmonoide)

  //------------SECONDO ESEMPIO-------------
  val set1  = Set.range(0, 99).map(i => i:Any)
  def + (x:Any,y:Any) : Any = (x.asInstanceOf[Int] + y.asInstanceOf[Int]) % 99  
  val id1 = 0
  val monoid1 = new Monoid(set1, +, id1)
  println("identita funziona? " + monoid1.identità)
  println("chiusura funziona? " + monoid1.chiusura)
  println("associatività funziona? " + monoid1.associatività)
  println("overall : "+ monoid1.èmonoide)

}
