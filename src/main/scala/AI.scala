package CombatSim.AI

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 9:37 PM
 * To change this template use File | Settings | File Templates.
 */

import CombatSim.CharacterSheet.WeaponSkill
import CombatSim.Fighter.Fighter
import CombatSim.Maneuver._
import Matrix._
import Matrix._
import Row.Row

class AI(id: Int) {
  val ID = id
  // start of with a uniform 11x7 matrix
  var strategyMatrix = for(i <- List.range(0, 7)) yield for(j <- List.range(0, 11)) yield 1.0

  def mutateStrategy() {
    strategyMatrix = for(row <- strategyMatrix) yield for(entry <- row) yield entry * (util.Random.nextGaussian() / 8 + 1).min(0)
  }

  // select one of the maneuvers from CombatSim.Maneuver
  def chooseManeuver(self: Fighter, opponent: Fighter) = {

    // helper function to index the maneuvers
    def maneuverN(m: Maneuver) = m match {
      case m: AllOutAttack => 0
      case m: AllOutDefense => 1
      case m: Attack => 2
      case m: Evaluate => 3
    }

    // set up event vector
    // start with the opponents previous maneuver
    var eventV: Row = (for(i <- 0 until 4) yield if(i == maneuverN(opponent.maneuver)) 1. else 0.).toList
    // prepend own previous maneuver
    eventV :::= (for(i <- 0 until 4) yield if(i == maneuverN(self.maneuver)) 1. else 0.).toList
    // opponents current HP
    eventV ::= math.max(0, opponent.curHP)
    // then own current HP
    eventV ::= math.max(0, self.curHP)
    // if there are shock penalties (or boni from evaluate), we need to take them into account
    // the +5 to keep the value positive
    eventV ::= 5 + self.temporaryModifiers.values.flatten.filter(_.target == WeaponSkill).map(_.value).sum
    // which leaves us with an event vector with 11 members

    // multiply this vector with our strategy matrix to get our decision vector
    // since we have an event vector of 11 elements and an decision vector of 7,
    // our matrix needs to be of size 11x7

    val decisionVector = strategyMatrix * eventV

    // actually decide on one maneuver now

    val decisionP = util.Random.nextDouble() * decisionVector.sum

    def getDecision(decisionP: Double, decisionVector: Row): Int = {
      var sum = 0.001
      var i = 0
      for(element <- decisionVector) {
        sum += element
        if (decisionP <= sum)
          return i
        i += 1
      }
      // should never get here
      -1
    }

    // now that's the maneuver we ultimately use
    getDecision(decisionP, decisionVector) match {
      case 0 => new Attack
      case 1 => new AllOutAttackDetermined
      case 2 => new AllOutAttackDouble
      case 3 => new AllOutAttackStrong
      case 4 => new AllOutDefenseDouble
      case 5 => new AllOutDefenseIncreased
      case 6 => new Evaluate
    }
  }


  // input is a list of available attacks this turn, each with the relevant mods
  // return a list of the chosen attacks from CombatSim.Attack
  def chooseAttacks(availableAttacks: List[CombatSim.Attack.AttackModifiers]) = {
    // for rapid strike and dual-weapon attack, simple return and additional attack class with the appropriate modifiers
    // they will be executed in the order they appear in the list
    for(attackMods <- availableAttacks) yield new CombatSim.Attack.BasicAttack(attackMods, new CombatSim.HitLocation.Untargeted)
  }

}