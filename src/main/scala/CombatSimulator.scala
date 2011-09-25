package CombatSim.CombatSimulator

import CombatSim.Fighter.Fighter
import CombatSim.AI.AI
import CombatSim.CharacterSheet._
import CombatSim.Messages._
import akka.actor.Actor

class CombatSimulator extends Actor {
  def receive = {
    case SimulateCombat(fighters, ais, nrOfSimulations) =>
    val fighterStats = new collection.mutable.HashMap[String, Int]()  { override def default(key: String) = 0 }
    val AIStats = new collection.mutable.HashMap[Int, Int]() { override def default(key: Int) = 0 }
      // simulate requested number of fights, each with results in an Array
      // where for each fighter there's either a 'true' entry for alive or
      // false for dead
      for (i <- 0 until nrOfSimulations)
      {
        // give each fighter a random AI
        for(f <- fighters)
          f.AI = ais(util.Random.nextInt(100))

        // let the fight
        val result = fight(fighters)

      // now let's tally up all those deaths
        for(r <- result) {
          fighterStats(r._1) += (if (r._3) 1 else 0)
          AIStats(r._2) += (if (r._3) 1 else 0)
        }
      }

      // send the normalized result back to our work master
      self reply CombatResult( fighterStats.toMap, AIStats.toMap )
  }

  def fight(unsortedFighters: Array[Fighter]) = {
    // sort players in order of basic speed
    val fighters = unsortedFighters.sortWith((a, b) => a.charsheet(BS) > b.charsheet(BS))

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
        round += 1
      }

      val attacker = fighters(i)
      val defender = fighters((i + 1) % numF)


      // tell the fighter that his turn just began, so certain events can take place
      // like HT rolls for <0 HP
      // only let him act if he still can
      if (attacker.startTurn())
      {
        // for now, fighters don't do anything else than attack
        // but they can use different maneuvers
        // TODO: implement multiple maneuvers per turn
        // give attacker information about his opponent
        attacker.chooseManeuver(defender)

        // find out which attacks are provided by the maneuvers
        val availableAttacks = attacker.maneuver.attack(attacker)

        // tell the fighter how many and which attacks he has
        val chosenAttacks = attacker.chooseAttacks(availableAttacks)

        // right now, there are only two fighters and they can only attack each
        // other, so no targeting here
        // TODO: take reach into account
        chosenAttacks.foreach(_.attack(attacker, defender))

      }

      // tell the player that the his turn just ended, so effects like shock
      // penalties can be removed
      attacker.endTurn()

      // increment counter to next fighter
      i = (i + 1) % numF
    }

    // return a list of (name, AI ID, alive status) for the fighters
    fighters.map(f => (f.charsheet.name, f.AI.ID, f.dead)).toList
  }
}