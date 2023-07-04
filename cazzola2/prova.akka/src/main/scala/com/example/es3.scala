import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props

class SimpleActor extends Actor {
        def receive = {
            case (pid, s: String) => println(self + " : "+ s)
            case i : Int => println("number : "+i)
        }
        def foo = println("normal method")
}

object es3 extends App{
    
    val system = ActorSystem("SimpleSystem")
    val actor = system.actorOf(Props[SimpleActor],"SimpleActor")
    println("inizio messaggi")
    actor ! (self, "hey baby")
    actor ! "ce l'ho fatta"
    println("fine messaggi messaggi")
}