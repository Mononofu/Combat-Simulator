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
      // simulate requested number of fights, each with results in an Array
      // where for each fighter there's either a 'true' entry for alive or
      // false for dead
      val battles = for (i <- 0 until nrOfSimulations) yield fight(fighters)

      // now let's tally up all those deaths
      // TODO: generalize this to n fighters, and not just 2
      val totalDeaths = battles.foldLeft(List(0, 0))((total, status) => List(total(0) + (if (status(0)) 0 else 1), total(1) + (if (status(1)) 0 else 1)))

      // send the normalized result back to our work master
      self reply CombatResult(totalDeaths.map(_ * 1. / nrOfSimulations))
  }

  def log(str: String = "") {
    // centralized output so it can easily be disabled, mostly for debugging
    //println(str)
  }

  def fight(unsortedFighters: Array[Fighter]) = {
    // sort players in order of basic speed
    val fighters = unsortedFighters.sortWith((a, b) => a.BS > b.BS)

    // reset their HP and alive status
    fighters.foreach(_.reset())

    var i = 0
    var round = 1

    // how many fighters? mostly to remove visual clutter
    val numF = fighters.length

    // simulate untily only one player is alive
    while (fighters.filter(_.alive).length > 1) {
      // if it's fighter 0's turn, we probably just begun a new turn
      if (i == 0) {
        log()
        // let's display HP for each fighter
        // actually, this will appear as belonging to the previous round
        for (player <- fighters) {
          player.log("%d HP  ".format(player.curHP))
        }
        log()

        // banner to start new round
        log("******* round %2d *******".format(round))
        round += 1
      }

      // tell the fighter that his turn just began, so certain events can take place
      // like HT rolls for <0 HP
      fighters(i).startTurn()

      // for now, fighters don't do anything else than attack
      // TODO: add more maneuvers
      if (fighters(i).attack())
        // right now, there are only two fighters and they can only attack each
        // other, so now targeting here
        if (!fighters((i + 1) % numF).defend())
        {
          fighters((i + 1) % numF).receiveDamage(fighters(i).doDamage())
        }

      // tell the player that the his turn just ended, so effects like shock
      // penalties can be removed
      fighters(i).endTurn()

      // increment counter to next fighter
      i = (i + 1) % numF
    }

    // at the end of combat, display HP and alive status for each fighter
    // will only be displayed if println is uncommented in the logging function
    for (fighter <- fighters) {
      if (!fighter.alive) {
        log("%s: *dead*".format(fighter.name))
      }
      else {
        log("%s: %d HP".format(fighter.name, fighter.curHP))
      }
    }

    // return an array of alive status for the fighters
    fighters.map(_.alive)
  }
}