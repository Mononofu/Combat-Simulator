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

class WorkMaster(
  nrOfWorkers: Int, nrOfJobs: Int, nrOfSimulations: Int, latch: CountDownLatch)
  extends Actor {

  var nrOfResults: Int  = _
  var start      : Long = _
  var results = List[List[Double]]()


  // akka code from online documentation
  // create the workers
  val workers = Vector.fill(nrOfWorkers)(actorOf[CombatSimulator].start())

  // wrap them with a load-balancing router
  val router = Routing.loadBalancerActor(CyclicIterator(workers)).start()

  // message handler
  def receive = {
    case Simulate =>
      // schedule work
      //for (start <- 0 until nrOfMessages) router ! Work(start, nrOfElements)
      for (i <- 0 until nrOfJobs) router !
                                     SimulateCombat(Array(
                                                           Fighter(20, Dice(1, 3), 12, 15, 9, 6., "me"),
                                                           Fighter(13, Dice(2, 1), 18, 11, 8, 5.75, "you")),
                                                     nrOfSimulations / nrOfJobs)

      // send a PoisonPill to all workers telling them to shut down themselves
      router ! Broadcast(PoisonPill)

      // send a PoisonPill to the router, telling him to shut himself down
      router ! PoisonPill

    case CombatResult(result) =>
      // handle result from the worker
      results ::= result
      nrOfResults += 1
      if (nrOfResults == nrOfJobs) self.stop()
  }

  override def preStart() {
    start = System.currentTimeMillis
  }

  override def postStop() {
    // tell the world that the calculation is complete
    results.foldLeft(List(0., 0.))((total, res) => List(total(0) + res(0), total(1) + res(1))).map(_ / nrOfJobs).foreach( r => println("%.2f %% deaths".format(r * 100.)) )
    println(
             "\n\tCalculation time: \t%s millis"
             .format(System.currentTimeMillis - start))
    latch.countDown()
  }
}
