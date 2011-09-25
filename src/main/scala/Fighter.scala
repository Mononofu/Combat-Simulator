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
import CombatSim.CharacterSheet._


case class Fighter(charsheet: CharacterSheet) {
  var dead               = false
  var curHP              = charsheet(HP)
  var maneuver: Maneuver = new Attack
  val DR                 = 0
  var AI = new CombatSim.AI.AI(-1)
  var temporaryModifiers = new collection.mutable.HashMap[ModifierType, List[Modifier]]()

  def attack(mod: Int = 0) = DefaultDice.check(charsheet(WeaponSkill) + mod + temporaryModifiers.values.flatten.filter(_.target == WeaponSkill).map(_.value).sum)

  def chooseManeuver(opponent: Fighter) = AI.chooseManeuver(this, opponent)

  def chooseAttacks(availableAttacks: List[AttackModifiers]) = AI.chooseAttacks(availableAttacks)

  def doDamage(mod: Int = 0) = {
    val dmg = charsheet.swingDamage.roll() + mod

    // this returns a case class: Damage(actual damage, multiplier for damage type / hit location)
    Cutting.calcDamage(Damage(dmg, 1.))
  }

  def parry(mod: Int = 0) = DefaultDice.check(charsheet(Parry) + mod + temporaryModifiers.values.flatten.filter(_.target == Parry).map(_.value).sum)

  def dodge(mod: Int = 0) = DefaultDice.check(charsheet(Dodge) + mod + temporaryModifiers.values.flatten.filter(_.target == Dodge).map(_.value).sum)

  // TODO: add option for retreat
  def defend(mod: Int = 0) = {
    if (charsheet(Parry) >= charsheet(Dodge))
      parry(mod)
    else
      dodge(mod)
  }

  // return true if we are still alive and can act
  // false if we can't act (dead, stunned, etc)
  def startTurn() = {
    if (curHP < 0) {
      // need to make HT check every turn now
      HTCheck(-(curHP.abs / charsheet(HP)))
    }
    else
      true
  }

  def endTurn() {
    // decrease duration of all modifiers by one turn
    temporaryModifiers.foreach(_._2.foreach(_.duration -= 1))

    // remove those which no longer apply
    temporaryModifiers = temporaryModifiers.filter(_._2.forall(_.duration > 0))
  }


  def HTCheck(mod: Int = 0) = DefaultDice.check(charsheet(HT)) match {
    case Failure | CriticalFailure =>
      dead = true
      false
    case _ =>
      true
  }

  def receiveDamage(damage: Damage) {
    if (damage.baseDamage >= 1) {
      // TODO: take hitlocation specific DR into account
      val injury = math.max((damage.baseDamage - DR) * damage.multiplier, 1).toInt

      if (injury > charsheet(HP) / 2) HTCheck()

      // crossing HP treshold because of damage?
      // TODO: do something about crossing multiple thresholds at once
      if ((curHP - injury) < 0 && curHP / charsheet(HP) != (curHP - injury) / charsheet(HP)) HTCheck()

      //actually apply the damage to the HP
      curHP -= injury

      var shockPenalties = 0
      // only affect DX- and IQ-based skills, but not defenses
      if (charsheet(HP) >= 20)
        shockPenalties = math.max(math.min(injury / (charsheet(HP) / 10), 4), shockPenalties)
      else
        shockPenalties = math.max(math.min(injury, 4), shockPenalties)

      temporaryModifiers.getOrElseUpdate(ShockPenalties, List(Modifier(WeaponSkill, -shockPenalties, 1))) match {
        case mod => if (mod.head.value < shockPenalties) temporaryModifiers.update(ShockPenalties, List(Modifier(WeaponSkill, -shockPenalties, 1)))
      }

    }
  }

  def alive = !dead

  def reset() {
    curHP = charsheet(HP)
    dead = false
    temporaryModifiers.clear()
  }
}