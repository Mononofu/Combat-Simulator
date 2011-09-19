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
import CombatSim.DamageType._

case class Fighter(weaponSkill: Int, damage: Dice, HP: Int, HT: Int, dodgeScore: Int, BS: Double, name: String) {
  val parryScore     = 3 + weaponSkill / 2
  var dead           = false
  var curHP          = HP
  var shockPenalties = 0
  var maneuver       = new Attack

  def attack(mod: Int = 0) = {
    // TODO: add hitlocations
    val roll = DefaultDice.roll()
    if (roll <= (weaponSkill - shockPenalties + mod)) {
      log("%d < %d => hit".format(roll, (weaponSkill - shockPenalties + mod)))
      true
    } else {
      log("%d > %d => miss".format(roll, (weaponSkill - shockPenalties + mod)))
      false
    }
  }

  def chooseManeuver() = {

  }

  def doDamage(mod: Int = 0) = {
    // TODO: incorporate damage type (imp, pi++, etc)
    val dmg = damage.roll() + mod
    log("%d dmg".format(dmg))

    // this is (actual damage, multiplier for damage type / hit location)
    Damage(dmg, 1.)
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
    shockPenalties = 0
  }

  def log(str: String) {
    //println("%4s: %s".format(name, str))
  }

  def receiveDamage(damage: Damage) {
    if (damage.baseDamage >= 1) {
      val injury = math.max(damage.baseDamage * damage.multiplier, 1).toInt
      
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
      // TODO: take different damage types into account
      curHP -= injury

      // only affect DX- and IQ-based skills, but not defenses
      if (HP >= 20) {
        shockPenalties = math.max(math.min(injury / (HP / 10), 4), shockPenalties)
      }
      else {
        shockPenalties = math.max(math.min(injury, 4), shockPenalties)
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