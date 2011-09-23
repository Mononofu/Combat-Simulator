package CombatSim.CharacterSheet

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/23/11
 * Time: 12:52 PM
 * To change this template use File | Settings | File Templates.
 */

sealed trait stat

case object ST extends stat
case object DX extends stat
case object IQ extends stat
case object HT extends stat

case object HP extends stat
case object FP extends stat
// sincee BS is not used in calculation, it's just for ordering
// so to fit it into an Int, just multiply by 4
case object BS extends stat

case object Dodge extends stat
case object Parry extends stat
case object Block extends stat

case object WeaponSkill extends stat
// bonus dmg above normal ST dmg
// from weapon or advantages
case object DamageBonus extends stat

import CombatSim.Tools._

case class CharacterSheet(name: String, customStats: (stat, Int)*) {
  
  val stats = Map[stat, Int](ST -> 10, DX -> 10, IQ -> 10, HT -> 10,
                                        HP -> 10, FP -> 10, BS -> 5,
                                        Dodge -> 8, Parry -> 0, Block -> 0,
                                        WeaponSkill -> 0, DamageBonus -> 0) ++ customStats.toMap

  // this is only swing damage
  val swingDamage = stats(ST) match {
    case st if st < 10 => Dice(1, (st + 1) / 2 - 6)
    case st if st < 28 => Dice( (st - 5) / 4, ((st - 1) % 4) - 1)
    case st if st < 50 => Dice( (st + 17) / 8, (((st + 1) * 2 ) % 4) - 1)
    case st => Dice(st / 10 + 3, 0)
  }

  def apply(s: stat) = stats(s)
}