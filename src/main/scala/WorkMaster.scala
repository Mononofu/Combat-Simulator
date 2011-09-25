package CombatSim.WorkMaster

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 7:36 PM
 * To change this template use File | Settings | File Templates.
 */

import akka.actor.{Actor, PoisonPill}
import Actor._
import akka.routing.{Routing, CyclicIterator}
import Routing._
import java.util.concurrent.CountDownLatch

import CombatSim.Fighter.Fighter
import CombatSim.Tools._
import CombatSim.Messages._
import CombatSim.CombatSimulator.CombatSimulator
import CombatSim.CharacterSheet.CharacterSheet
import CombatSim.AI.AI

class WorkMaster(
  nrOfWorkers: Int, nrOfJobs: Int, nrOfSimulations: Int, latch: CountDownLatch, fighters: CharacterSheet*)
  extends Actor {

  var nrOfResults: Int  = _
  var start      : Long = _
  val fighterStats = new collection.mutable.HashMap[String, Int]()  { override def default(key: String) = 0 }
  val AIStats = new collection.mutable.HashMap[Int, Int]() { override def default(key: Int) = 0 }
  var nrOfMessagesSent = 0


  // akka code from online documentation
  // create the workers
  val workers = Vector.fill(nrOfWorkers)(actorOf[CombatSimulator].start())

  // wrap them with a load-balancing router
  val router = Routing.loadBalancerActor(CyclicIterator(workers)).start()

  // message handler
  def receive = {
    case Simulate =>
      // create AIs
      val nrOfAIs = 100
      val AIs = for(i <- 0 until nrOfAIs) yield new AI(i)

      // match our fighters in all possible combinations
      // each combination will be run nrOfSimulation times
      val matches = fighters.combinations(2)

      // schedule work
      for (combo <- matches)
        for (i <- 0 until nrOfJobs) {
          router ! SimulateCombat(combo.map(Fighter(_)).toArray, AIs.toArray, nrOfSimulations / nrOfJobs)
          nrOfMessagesSent += 1
        }

      // send a PoisonPill to all workers telling them to shut down themselves
      router ! Broadcast(PoisonPill)

      // send a PoisonPill to the router, telling him to shut himself down
      router ! PoisonPill

    case CombatResult(fStats, aiStats) =>
      for( (name, deaths) <- fStats) {
        fighterStats(name) += deaths
      }

      for ( (id, deaths) <- aiStats ) {
        AIStats(id) += deaths
      }

      nrOfMessagesSent -= 1
      if (nrOfMessagesSent == 0) self.stop()
  }

  override def preStart() {
    start = System.currentTimeMillis
  }

  override def postStop() {
    // tell the world that the calculation is complete
    val totalDeaths = fighterStats.toList.map(_._2).sum
    println("\tFighters:")
    for( (name, deaths) <- fighterStats.toList.sortBy(_._2)) {
      println("%10s \t %.2f%% \t %d".format(name, deaths * 100. / totalDeaths, deaths))
    }

    println("\tAIs:")
    for( (id, deaths) <- AIStats.toList.sortBy(_._2)) {
      println("%d \t\t\t %.2f%% \t %d".format(id, deaths * 100. / totalDeaths, deaths))
    }

    println("\n\tCalculation time: \t%s millis".format(System.currentTimeMillis - start))
    latch.countDown()
  }
}
