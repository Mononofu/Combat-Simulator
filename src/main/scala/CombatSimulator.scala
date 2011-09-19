package CombatSim.CombatSimulator

import CombatSim.Fighter.Fighter
import CombatSim.Messages._
import akka.actor.Actor

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 7:33 PM
 * To change this template use File | Settings | File Templates.
 */

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

  def fight(unsortedPlayers: Array[Fighter]) = {
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