package CombatSim.AI

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 9:37 PM
 * To change this template use File | Settings | File Templates.
 */

import CombatSim.Fighter.Fighter

class AI {
  // select one of the maneuvers from CombatSim.Maneuver
  def chooseManeuver(self: Fighter, opponent: Fighter) = new CombatSim.Maneuver.Attack

  // input is a list of available attacks this turn, each with the relevant mods
  // return a list of the chosen attacks from CombatSim.Attack
  def chooseAttacks(availableAttacks: List[CombatSim.Attack.AttackModifiers]) = {
    // for rapid strike and dual-weapon attack, simple return and additional attack class with the appropriate modifiers
    // they will be executed in the order they appear in the list
    for(attackMods <- availableAttacks) yield new CombatSim.Attack.BasicAttack(attackMods, new CombatSim.HitLocation.Untargeted)
  }

}