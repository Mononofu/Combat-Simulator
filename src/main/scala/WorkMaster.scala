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

class WorkMaster(
  nrOfWorkers: Int, nrOfJobs: Int, nrOfSimulations: Int, latch: CountDownLatch, fighters: CharacterSheet*)
  extends Actor {

  var nrOfResults: Int  = _
  var start      : Long = _
  val results           = collection.mutable.HashMap[List[String], List[Int]]()


  // akka code from online documentation
  // create the workers
  val workers = Vector.fill(nrOfWorkers)(actorOf[CombatSimulator].start())

  // wrap them with a load-balancing router
  val router = Routing.loadBalancerActor(CyclicIterator(workers)).start()

  // message handler
  def receive = {
    case Simulate =>
      // match our fighters in all possible combinations
      // each combination will be run nrOfSimulation times
      val matches = fighters.combinations(2)

      // schedule work
      for (combo <- matches)
        for (i <- 0 until nrOfJobs)
          router ! SimulateCombat(combo.map(Fighter(_)).toArray, nrOfSimulations / nrOfJobs)

      // send a PoisonPill to all workers telling them to shut down themselves
      router ! Broadcast(PoisonPill)

      // send a PoisonPill to the router, telling him to shut himself down
      router ! PoisonPill

    case CombatResult(result) =>
      // handle result from the worker
      results(result._1) = results.getOrElse(result._1, (for(i <- 0 until result._2.length) yield 0).toList).zip(result._2).map(a => a._1 + a._2)
      nrOfResults += 1
      if (nrOfResults == nrOfJobs) self.stop()
  }

  override def preStart() {
    start = System.currentTimeMillis
  }

  override def postStop() {
    // tell the world that the calculation is complete
    for( (fighters, result) <- results) {
      print("Fighters:")
      fighters.foreach(name => print("\t\t" + name))
      println()
      print("Deaths:\t")
      result.foreach(r => print("\t\t%d".format(r)))
      println("")
      print("\t\t")
      result.foreach(r => print("\t\t%.2f %%".format(r * 100. / nrOfSimulations)))
    }

    //results.foldLeft(List(0., 0.))((total, res) => List(total(0) + res(0)._2, total(1) + res(1)._2)).map(_ / nrOfJobs).foreach(r => println("%.2f %% deaths".format(r * 100.)))
    println(
             "\n\tCalculation time: \t%s millis"
             .format(System.currentTimeMillis - start))
    latch.countDown()
  }
}
