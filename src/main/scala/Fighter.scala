package CombatSim.Fighter

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 7:30 PM
 * To change this template use File | Settings | File Templates.
 */

import CombatSim.Tools._
import CombatSim.Maneuver._
import CombatSim.Attack._
import CombatSim.DamageType._

sealed trait ModifierType

// we need this to determine the source of the modifiers
// since shock penalties don't stack
case object ShockPenalties extends ModifierType



//
// TODO: extend this to general modifiers using HashMap
case class Modifier(var hitPenalty: Int, var duration: Int)

case class Fighter(weaponSkill: Int, damage: Dice, HP: Int, HT: Int, dodgeScore: Int, BS: Double, name: String) {
  val parryScore     = 3 + weaponSkill / 2
  var dead           = false
  var curHP          = HP
  var maneuver       = new Attack
  val DR = 0
  val AI = new CombatSim.AI.AI
  var temporaryModifiers = new collection.mutable.HashMap[ModifierType, Modifier]()

  def attack(mod: Int = 0) = DefaultDice.roll() <= (weaponSkill - (temporaryModifiers.foldLeft(0)((sum, mod) => sum + mod._2.hitPenalty)) + mod)

  def chooseManeuver() = AI.chooseManeuver()

  def chooseAttacks(availableAttacks: List[AttackModifiers]) = AI.chooseAttacks(availableAttacks)

  def doDamage(mod: Int = 0) = {
    val dmg = damage.roll() + mod
    log("%d dmg".format(dmg))

    // this returns a case class: Damage(actual damage, multiplier for damage type / hit location)
    Cutting.calcDamage(Damage(dmg, 1.))
  }

  def parry(mod: Int = 0) = {
    if (DefaultDice.check(parryScore + mod)) {
      log("parried")
      true
    }
    else {
      false
    }
  }

  def dodge(mod: Int = 0) = {
    if (DefaultDice.check(dodgeScore + mod)) {
      log("dodged")
      true
    }
    else {
      false
    }
  }

  def defend(mod: Int = 0) = {
    if (parryScore >= dodgeScore)
      parry(mod)
    else
      dodge(mod)
  }

  def startTurn() {
    if (curHP < 0) {
      // need to make HT check every turn now
      log("HT check at start of round")
      if (!DefaultDice.check(HT - (curHP.abs / HP))) {
        dead = true
        log("failed")
      }
      else {
        log("succeeded")
      }
    }
  }

  def endTurn() {
    // decrease duration of all modifiers by one turn
    temporaryModifiers.foreach(_._2.duration -= 1)
    
    // remove those which no longer apply
    temporaryModifiers = temporaryModifiers.filter(_._2.duration > 0)
  }

  def log(str: String) {
    //println("%4s: %s".format(name, str))
  }

  def receiveDamage(damage: Damage) {
    if (damage.baseDamage >= 1) {
      // TODO: take hitlocation specific DR into account
      val injury = math.max((damage.baseDamage - DR) * damage.multiplier, 1).toInt
      
      if (injury > HP / 2) {
        log("major wound")
        if (!DefaultDice.check(HT)) {
          log("HT check failed")
          dead = true
        }
        else {
          log("HT check succeeded")
        }
      }

      // crossing HP treshold because of damage?
      // TODO: do something about crossing multiple thresholds at once
      if ((curHP - injury) < 0 && curHP / HP != (curHP - injury) / HP) {
        if (DefaultDice.check(HT)) {
          log("HT check succeeded")
        }
        else {
          log("HT check failed")
          dead = true
        }

      }


      //actually apply the damage to the HP
      curHP -= injury

	  var shockPenalties = 0
      // only affect DX- and IQ-based skills, but not defenses
      if (HP >= 20) {
        shockPenalties = math.max(math.min(injury / (HP / 10), 4), shockPenalties)
      }
      else {
        shockPenalties = math.max(math.min(injury, 4), shockPenalties)
      }
      
      temporaryModifiers.getOrElseUpdate(ShockPenalties, Modifier(shockPenalties, 1)) match {
        case mod => if (mod.hitPenalty < shockPenalties) temporaryModifiers.update(ShockPenalties, Modifier(shockPenalties, 1))
      }

    }
  }

  def alive = {
    !dead
  }

  def reset() {
    curHP = HP
    dead = false
  }
}