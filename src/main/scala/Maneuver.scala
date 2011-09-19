package CombatSim.Maneuver

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 7:59 PM
 * To change this template use File | Settings | File Templates.
 */

import CombatSim.Fighter.Fighter

abstract class Maneuver {
  def attack(attacker: Fighter, defender: Fighter, attackType: CombatSim.Attack.Attack) = attackType.attack(attacker, defender)
  def defend(attacker: Fighter, defender: Fighter, mod: Int = 0) = defender.defend(mod)
}

class Attack extends Maneuver {}

abstract class AllOutAttack extends Maneuver {
  override def defend(attacker: Fighter, defender: Fighter, mod: Int = 0) = false
}

class AllOutAttackDetermined extends AllOutAttack {
  override def attack(attacker: Fighter, defender: Fighter, attackType: CombatSim.Attack.Attack) = attackType.attack(attacker, defender, +4)
}

class AllOutAttackDouble extends AllOutAttack {
  override def attack(attacker: Fighter, defender: Fighter, attackType: CombatSim.Attack.Attack) = {
    for(i <- 0 until 2)
      attackType.attack(attacker, defender, -4*i)
  }
}

class AllOutAttackStrong extends AllOutAttack {
  override def attack(attacker: Fighter, defender: Fighter, attackType: CombatSim.Attack.Attack) = attackType.attack(attacker, defender, 0, math.max(2, attacker.damage.numDice))
}

abstract class AllOutDefense extends Maneuver {
  override def attack(attacker: Fighter, defender: Fighter, attackType: CombatSim.Attack.Attack) = false
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

