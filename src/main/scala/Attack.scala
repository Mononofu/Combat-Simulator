package CombatSim.Attack

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 9:12 PM
 * To change this template use File | Settings | File Templates.
 */

import CombatSim.Fighter.Fighter

abstract class Attack {
  def attack(attacker: Fighter, defender: Fighter, hitMod: Int = 0, dmgMod: Int = 0) = {
    if (attacker.attack(hitMod))
        if (!defender.maneuver.defend(attacker, defender))
          defender.receiveDamage(attacker.doDamage(dmgMod))
  }
}

class DeceptiveAttack(level: Int) extends Attack {
  override def attack(attacker: Fighter, defender: Fighter, hitMod: Int = 0, dmgMod: Int = 0) = {
    if (attacker.attack(-2 * level + hitMod))
        if (!defender.maneuver.defend(attacker, defender, -level))
          defender.receiveDamage(attacker.doDamage(dmgMod))
  }
}

class TelegraphicAttack extends Attack {
  override def attack(attacker: Fighter, defender: Fighter, hitMod: Int = 0, dmgMod: Int = 0) = {
    if (attacker.attack(+4))
        if (!defender.maneuver.defend(attacker, defender, +2))
          defender.receiveDamage(attacker.doDamage(dmgMod))
  }
}