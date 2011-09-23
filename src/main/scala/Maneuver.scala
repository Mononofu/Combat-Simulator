package CombatSim.Maneuver

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 7:59 PM
 * To change this template use File | Settings | File Templates.
 */

import CombatSim.Fighter._
import CombatSim.Attack.AttackModifiers
import CombatSim.Tools.Failure

abstract class Maneuver {
  // return number of attacks with modifiers for each
  def attack(attacker: Fighter) = List(AttackModifiers())

  def defend(attacker: Fighter, defender: Fighter, mod: Int = 0) = defender.defend(mod)
}

class Attack extends Maneuver {}

abstract class AllOutAttack extends Maneuver {
  override def defend(attacker: Fighter, defender: Fighter, mod: Int = 0) = Failure
}

class AllOutAttackDetermined extends AllOutAttack {
  override def attack(attacker: Fighter) = List(AttackModifiers(+4, +0))
}

class AllOutAttackDouble extends AllOutAttack {
  override def attack(attacker: Fighter) = List(AttackModifiers(), AttackModifiers())
}

class AllOutAttackStrong extends AllOutAttack {
  override def attack(attacker: Fighter) = List(AttackModifiers(+0, math.max(2, attacker.charsheet.swingDamage.numDice)))
}

abstract class AllOutDefense extends Maneuver {
  override def attack(attacker: Fighter) = List()
}

class AllOutDefenseIncreased extends AllOutDefense {
  override def defend(attacker: Fighter, defender: Fighter, mod: Int = 0) = defender.defend(+2 + mod)
}

class AllOutDefenseDouble extends AllOutDefense {
  override def defend(attacker: Fighter, defender: Fighter, mod: Int = 0) = {
    import CombatSim.CharacterSheet._
    if (defender.charsheet(Parry) >= defender.charsheet(Dodge)) {
      defender.parry(mod) match {
        // if our parry simply fails, we can try again
        case Failure => defender.dodge(mod)
        case result => result
      }
    }
    else {
      defender.dodge(mod)match {
        case Failure => defender.parry(mod)
        case result => result
      }
    }
  }
}

class Evaluate extends Maneuver {
  override def attack(attacker: Fighter) = {
    // do we already have some evaluate bonus?
    val curMod = attacker.temporaryModifiers.getOrElse(EvaluateModifier, Modifier(HitPenalty, 0, 2))

    // max bonus can be +3, so limit it, but refresh the duration counter
    attacker.temporaryModifiers.put(EvaluateModifier, Modifier(HitPenalty, math.min(curMod.value+1, 3), 2))

    // can't attack while evaluating
    List()
  }
}