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
  def attack(attacker: Fighter, defender: Fighter) = {
    if (attacker.attack())
        if (!defender.maneuver.defend(attacker, defender))
          defender.receiveDamage(attacker.doDamage())
  }
  def defend(attacker: Fighter, defender: Fighter) = defender.defend()

}

class Attack extends Maneuver {

}

abstract class AllOutAttack extends Maneuver {
  override def defend(attacker: Fighter, defender: Fighter) = false
}

class AllOutAttackDetermined extends AllOutAttack {
  override def attack(attacker: Fighter, defender: Fighter) = {
    if (attacker.attack(+4))
        if (!defender.maneuver.defend(attacker, defender))
          defender.receiveDamage(attacker.doDamage())
  }
}

class AllOutAttackDouble extends AllOutAttack {
  override def attack(attacker: Fighter, defender: Fighter) = {
    for(i <- 0 until 2)
      if (attacker.attack())
          if (!defender.maneuver.defend(attacker, defender))
            defender.receiveDamage(attacker.doDamage())
  } }

class AllOutAttackStrong extends AllOutAttack {
  override def attack(attacker: Fighter, defender: Fighter) = {
    if (attacker.attack())
        if (!defender.maneuver.defend(attacker, defender))
          defender.receiveDamage(attacker.doDamage(math.max(2, attacker.damage.numDice)))
  }
}

abstract class AllOutDefense extends Maneuver {
  override def attack(attacker: Fighter, defender: Fighter) = false
}

class AllOutDefenseIncreased extends Maneuver {
  override def defend(attacker: Fighter, defender: Fighter) = defender.defend(+2)
}

class AllOutDefenseDouble extends AllOutDefense {
  override def defend(attacker: Fighter, defender: Fighter) = defender.defend()
}

