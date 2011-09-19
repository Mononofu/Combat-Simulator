package CombatSim.Sim

import akka.actor.{Actor, PoisonPill}
import Actor._
import akka.routing.{Routing, CyclicIterator}
import Routing._
import akka.dispatch.Dispatchers

import java.util.concurrent.CountDownLatch

sealed trait SimMessage

case object Simulate extends SimMessage

case class SimulateCombat(fighters: Array[Player], nrOfSimulations: Int) extends SimMessage

case class CombatResult(result: List[Double]) extends SimMessage


case class Dice(numDice: Int = 3, mod: Int = 0) {
  def roll() = {
    mod + (for (i <- 0 until numDice) yield util.Random.nextInt(6) + 1).sum
  }
}

object DefaultDice extends Dice {
  def check(n: Int) = {
    if (roll() <= n) true else false
  }
}

case class Player(weaponSkill: Int, damage: Dice, HP: Int, HT: Int, dodge: Int, BS: Double, name: String) {
  val parry          = 3 + weaponSkill / 2
  var dead           = false
  var curHP          = HP
  var shockPenalties = 0

  def attack() = {
    val roll = DefaultDice.roll()
    if (roll <= (weaponSkill - shockPenalties)) {
      log("%d < %d => hit".format(roll, (weaponSkill - shockPenalties)))
      true
    } else {
      log("%d > %d => miss".format(roll, (weaponSkill - shockPenalties)))
      false
    }
  }

  def doDamage() = {
    val dmg = damage.roll()
    log("%d dmg".format(dmg))
    dmg
  }

  def defend() = {
    if (parry >= dodge) {
      if (DefaultDice.check(parry)) {
        log("parried")
        true
      }
      else {
        false
      }
    }
    else {
      if (DefaultDice.check(dodge)) {
        log("dodged")
        true
      }
      else {
        false
      }
    }
  }

  def startTurn() {
    if (curHP < 0) {
      // need to make HT check every turn now
      log("HT check at start of round")
      if (!DefaultDice.check(HT - (curHP.abs / HP))) {
        dead = true
        log("failed")
      }
      else {
        log("succeeded")
      }
    }
  }

  def endTurn() {
    shockPenalties = 0
  }

  def log(str: String) {
    //println("%4s: %s".format(name, str))
  }

  def receiveDamage(dmg: Int) {
    if (dmg > HP / 2) {
      log("major wound")
      if (!DefaultDice.check(HT)) {
        log("HT check failed")
        dead = true
      }
      else {
        log("HT check succeeded")
      }
    }

    // crossing HP treshold because of damage?
    // TODO: do something about crossing multiple thresholds at once
    if ((curHP - dmg) < 0 && curHP / HP != (curHP - dmg) / HP) {
      if (DefaultDice.check(HT)) {
        log("HT check succeeded")
      }
      else {
        log("HT check failed")
        dead = true
      }

    }

    curHP -= dmg

    // only affect DX- and IQ-based skills, but not defenses
    if (HP >= 20) {
      shockPenalties = math.max(math.min(dmg / (HP / 10), 4), shockPenalties)
    }
    else {
      shockPenalties = math.max(math.min(dmg, 4), shockPenalties)
    }
  }

  def alive = {
    !dead
  }

  def reset() {
    curHP = HP
    dead = false
  }
}

class CombatSimulator extends Actor {
  def receive = {
    case SimulateCombat(fighters, nrOfSimulations: Int) =>
      val battles = for (i <- 0 until nrOfSimulations) yield fight(fighters)
      val totalDeaths = battles.foldLeft(List(0, 0))((total, status) => List(total(0) + (if (status(0)) 0 else 1), total(1) + (if (status(1)) 0 else 1)))
      self reply CombatResult(totalDeaths.map(_ * 1. / nrOfSimulations))
  }

  def log(str: String = "") {
    //println(str)
  }

  def fight(unsortedPlayers: Array[Player]) = {
    val players = unsortedPlayers.sortWith((a, b) => a.BS > b.BS)
    players.foreach(_.reset())

    var i = 0
    var round = 1
    val numP = players.length
    while (players.filter(_.alive).length > 1) {
      if (i == 0) {
        log()
        for (player <- players) {
          player.log("%d HP  ".format(player.curHP))
        }
        log()
        log("******* round %2d *******".format(round))
        round += 1
      }
      players(i).startTurn()
      if (players(i).attack())
        if (!players((i + 1) % numP).defend())
          players((i + 1) % numP).receiveDamage(players(i).doDamage())
      players(i).endTurn()
      i = (i + 1) % numP
    }

    for (player <- players) {
      if (!player.alive) {
        log("%s: *dead*".format(player.name))
      }
      else {
        log("%s: %d HP".format(player.name, player.curHP))
      }
    }

    players.map(_.alive)
  }
}

class Master(
  nrOfWorkers: Int, nrOfJobs: Int, nrOfSimulations: Int, latch: CountDownLatch)
  extends Actor {

  var nrOfResults: Int  = _
  var start      : Long = _
  var results = List[List[Double]]()


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
                                                           Player(20, Dice(1, 3), 12, 15, 9, 6., "me"),
                                                           Player(13, Dice(2, 1), 18, 11, 8, 5.75, "you")),
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


object Interface extends App {

    val latch = new CountDownLatch(1)

    // create the master
    val master = actorOf(new Master(8, 24, 96000, latch)).start()

    // start the calculation
    master ! Simulate

    // wait for master to shut down
    latch.await()


}
