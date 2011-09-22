package CombatSim.Attack

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 9:12 PM
 * To change this template use File | Settings | File Templates.
 */

import CombatSim.Fighter.Fighter
import CombatSim.HitLocation._

case class AttackModifiers(hitMod: Int = 0, dmgMod: Int = 0)

class BasicAttack(mods: AttackModifiers = AttackModifiers(), hitloc: HitLocation = new Untargeted, defendMod: Int = 0) {
  def attack(attacker: Fighter, defender: Fighter) = {
    import CombatSim.Tools.ResultType._
    attacker.attack(mods.hitMod) match {
      case CriticalSuccess =>
        // no defense possible
        // TODO: critical hit table here
        val dmg = attacker.doDamage(mods.dmgMod)
        defender.receiveDamage(hitloc.calcDamage(dmg))

      case Success => defender.maneuver.defend(attacker, defender, defendMod) match {
        case CriticalFailure | Failure =>
          val dmg = attacker.doDamage(mods.dmgMod)
          defender.receiveDamage(hitloc.calcDamage(dmg))
        case _ =>
      }

      case CriticalFailure =>
        // TODO: put critical miss table here

      case _ =>
        // miss, nothing happens
    }
  }
}

class DeceptiveAttack(level: Int, mods: AttackModifiers = AttackModifiers(), hitloc: HitLocation = new Untargeted)
  extends BasicAttack(AttackModifiers(mods.hitMod -2 *level, mods.dmgMod), hitloc, -level) {}

class TelegraphicAttack(mods: AttackModifiers = AttackModifiers(), hitloc: HitLocation = new Untargeted)
  extends BasicAttack(AttackModifiers(mods.hitMod + 4, mods.dmgMod), hitloc, +2) {}


class Feint(mods: AttackModifiers = AttackModifiers(), hitloc: HitLocation = new Untargeted) {
  def attack(attacker: Fighter, defender: Fighter) = {
    // TODO: add code for the feint here
    // should penalties should be represented in a fighter as a tuple: (penalty, turns remaining)
    // which is updated at the end of each round and then removed if turns <= 0
  }
}


// for dual-weapon attack and rapid-strike, see the "chooseAttacks" method in AI