package CombatSim.Attack

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 9:12 PM
 * To change this template use File | Settings | File Templates.
 */

import CombatSim.Fighter.Fighter
import CombatSim.Maneuver.BaseAttack
import CombatSim.HitLocation._

class BasicAttack(mods: BaseAttack = BaseAttack(), hitloc: HitLocation = new Untargeted) {
  def attack(attacker: Fighter, defender: Fighter) = {
    if (attacker.attack(mods.hitMod))
      if (!defender.maneuver.defend(attacker, defender))
        defender.receiveDamage(attacker.doDamage(mods.dmgMod))
  }
}

class DeceptiveAttack(level: Int, mods: BaseAttack = BaseAttack(), hitloc: HitLocation = new Untargeted) extends BasicAttack(mods, hitloc) {
  override def attack(attacker: Fighter, defender: Fighter) = {
    if (attacker.attack(-2 * level + mods.hitMod))
      if (!defender.maneuver.defend(attacker, defender, -level))
        defender.receiveDamage(attacker.doDamage(mods.dmgMod))
  }
}

class TelegraphicAttack(mods: BaseAttack = BaseAttack(), hitloc: HitLocation = new Untargeted) extends BasicAttack(mods, hitloc) {
  override def attack(attacker: Fighter, defender: Fighter) = {
    if (attacker.attack(mods.hitMod + 4))
      if (!defender.maneuver.defend(attacker, defender, +2))
        defender.receiveDamage(attacker.doDamage(mods.dmgMod))
  }
}

class Feint(mods: BaseAttack = BaseAttack(), hitloc: HitLocation = new Untargeted) {
  def attack(attacker: Fighter, defender: Fighter) = {
    // TODO: add code for the feint here
    // should penalties should be represented in a fighter as a tuple: (penalty, turns remaining)
    // which is updated at the end of each round and then removed if turns <= 0
  }
}