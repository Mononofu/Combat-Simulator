package CombatSim.Fighter

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 7:30 PM
 * To change this template use File | Settings | File Templates.
 */

import CombatSim.Tools._

case class Fighter(weaponSkill: Int, damage: Dice, HP: Int, HT: Int, dodge: Int, BS: Double, name: String) {
  val parry          = 3 + weaponSkill / 2
  var dead           = false
  var curHP          = HP
  var shockPenalties = 0

  def attack() = {
    val roll = DefaultDice.roll()
    if (roll <= (weaponSkill - shockPenalties)) {
      log("%d < %d => hit".format(roll, (weaponSkill - shockPenalties)))
      true
    } else {
      log("%d > %d => miss".format(roll, (weaponSkill - shockPenalties)))
      false
    }
  }

  def doDamage() = {
    val dmg = damage.roll()
    log("%d dmg".format(dmg))
    dmg
  }

  def defend() = {
    if (parry >= dodge) {
      if (DefaultDice.check(parry)) {
        log("parried")
        true
      }
      else {
        false
      }
    }
    else {
      if (DefaultDice.check(dodge)) {
        log("dodged")
        true
      }
      else {
        false
      }
    }
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

  def receiveDamage(dmg: Int) {
    if (dmg > HP / 2) {
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
    if ((curHP - dmg) < 0 && curHP / HP != (curHP - dmg) / HP) {
      if (DefaultDice.check(HT)) {
        log("HT check succeeded")
      }
      else {
        log("HT check failed")
        dead = true
      }

    }

    curHP -= dmg

    // only affect DX- and IQ-based skills, but not defenses
    if (HP >= 20) {
      shockPenalties = math.max(math.min(dmg / (HP / 10), 4), shockPenalties)
    }
    else {
      shockPenalties = math.max(math.min(dmg, 4), shockPenalties)
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