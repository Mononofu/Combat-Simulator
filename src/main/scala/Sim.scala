package CombatSim.Sim


import akka.actor.Actor.actorOf
import java.util.concurrent.CountDownLatch
import CombatSim.WorkMaster.WorkMaster
import CombatSim.Messages.Simulate

object Interface extends App {

    val latch = new CountDownLatch(1)

    // create the master
    val master = actorOf(new WorkMaster(8, 24, 9600, latch)).start()

    // start the calculation
    master ! Simulate

    // wait for master to shut down
    latch.await()
}
