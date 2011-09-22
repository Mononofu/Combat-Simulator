package CombatSim.Fighter

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 7:30 PM
 * To change this template use File | Settings | File Templates.
 */

import CombatSim.Tools._
import ResultType._
import CombatSim.Maneuver._
import CombatSim.Attack._
import CombatSim.DamageType._

sealed trait ModifierType

// we need this to determine the source of the modifiers
// since shock penalties don't stack
case object ShockPenalties extends ModifierType


sealed trait ModifierTarget

case object HitPenalty extends ModifierTarget


// our modifier has three parts:
// 1) the target, ie what value is affected, eg DX or hit penalties
// 2) the actual value of the modifier
// 3) how many turns it should last, counter is always decremented at the end
//    of each turn. if it reaches 0, the modifier is discarded. 
case class Modifier(target: ModifierTarget, var value: Int, var duration: Int)

case class Fighter(weaponSkill: Int, damage: Dice, HP: Int, HT: Int, dodgeScore: Int, BS: Double, name: String) {
  val parryScore     = 3 + weaponSkill / 2
  var dead           = false
  var curHP          = HP
  var maneuver       = new Attack
  val DR = 0
  val AI = new CombatSim.AI.AI
  var temporaryModifiers = new collection.mutable.HashMap[ModifierType, Modifier]()

  def attack(mod: Int = 0) = DefaultDice.check(weaponSkill - (temporaryModifiers.filter(_._2.target == HitPenalty).foldLeft(0)((sum, keyVal) => sum + keyVal._2.value)) + mod)

  def chooseManeuver() = AI.chooseManeuver()

  def chooseAttacks(availableAttacks: List[AttackModifiers]) = AI.chooseAttacks(availableAttacks)

  def doDamage(mod: Int = 0) = {
    val dmg = damage.roll() + mod

    // this returns a case class: Damage(actual damage, multiplier for damage type / hit location)
    Cutting.calcDamage(Damage(dmg, 1.))
  }

  def parry(mod: Int = 0) = DefaultDice.check(parryScore + mod)

  def dodge(mod: Int = 0) = DefaultDice.check(dodgeScore + mod)

  def defend(mod: Int = 0) = {
    if (parryScore >= dodgeScore)
      parry(mod)
    else
      dodge(mod)
  }

  def startTurn() {
    if (curHP < 0) {
      // need to make HT check every turn now
      HTCheck(- (curHP.abs / HP))
    }
  }

  def endTurn() {
    // decrease duration of all modifiers by one turn
    temporaryModifiers.foreach(_._2.duration -= 1)
    
    // remove those which no longer apply
    temporaryModifiers = temporaryModifiers.filter(_._2.duration > 0)
  }


  def HTCheck(mod: Int = 0) = DefaultDice.check(HT) match {
          case Failure | CriticalFailure => dead = true
          case _ =>
        }

  def receiveDamage(damage: Damage) {
    if (damage.baseDamage >= 1) {
      // TODO: take hitlocation specific DR into account
      val injury = math.max((damage.baseDamage - DR) * damage.multiplier, 1).toInt
      
      if (injury > HP / 2) HTCheck()

      // crossing HP treshold because of damage?
      // TODO: do something about crossing multiple thresholds at once
      if ((curHP - injury) < 0 && curHP / HP != (curHP - injury) / HP) HTCheck()

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
      
      temporaryModifiers.getOrElseUpdate(ShockPenalties, Modifier(HitPenalty, shockPenalties, 1)) match {
        case mod => if (mod.value < shockPenalties) temporaryModifiers.update(ShockPenalties, Modifier(HitPenalty, shockPenalties, 1))
      }

    }
  }

  def alive = {
    !dead
  }

  def reset() {
    curHP = HP
    dead = false
    temporaryModifiers.clear()
  }
}