package CombatSim.Maneuver

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 7:59 PM
 * To change this template use File | Settings | File Templates.
 */

import CombatSim.Fighter.Fighter

case class BaseAttack(hitMod: Int = 0, dmgMod: Int = 0)

abstract class Maneuver {
  // return number of attacks with modifiers for each
  def attack(attacker: Fighter) = List(BaseAttack())

  def defend(attacker: Fighter, defender: Fighter, mod: Int = 0) = defender.defend(mod)
}

class Attack extends Maneuver {}

abstract class AllOutAttack extends Maneuver {
  override def defend(attacker: Fighter, defender: Fighter, mod: Int = 0) = false
}

class AllOutAttackDetermined extends AllOutAttack {
  override def attack(attacker: Fighter) = List(BaseAttack(+4, +0))
}

class AllOutAttackDouble extends AllOutAttack {
  override def attack(attacker: Fighter) = List(BaseAttack(), BaseAttack())
}

class AllOutAttackStrong extends AllOutAttack {
  override def attack(attacker: Fighter) = List(BaseAttack(+0, math.max(2, attacker.damage.numDice)))
}

abstract class AllOutDefense extends Maneuver {
  override def attack(attacker: Fighter) = List()
}

class AllOutDefenseIncreased extends Maneuver {
  override def defend(attacker: Fighter, defender: Fighter, mod: Int = 0) = defender.defend(+2 + mod)
}

class AllOutDefenseDouble extends AllOutDefense {
  override def defend(attacker: Fighter, defender: Fighter, mod: Int = 0) = {
    if (defender.parryScore >= defender.dodgeScore) {
      if( defender.parry(mod) )
        true
      else
        defender.dodge(mod)
    }
    else {
      if (defender.dodge(mod))
        true
      else
        defender.parry(mod)
    }
  }
}

