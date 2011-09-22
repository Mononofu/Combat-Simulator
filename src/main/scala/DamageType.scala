package CombatSim.DamageType

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 8:34 PM
 * To change this template use File | Settings | File Templates.
 */

case class Damage(baseDamage: Int, multiplier: Double) {
  def resolve() = (baseDamage * multiplier).toInt
}

abstract class  DamageType(mul: Double) {
  def calcDamage(dmg: Damage) = if(mul > dmg.multiplier) Damage(dmg.baseDamage, mul) else dmg
}

object SmallPiercing extends DamageType(0.5) {}

object Piercing extends DamageType(1.0) {}

object LargePiercing extends DamageType(1.5) {}

object HugePiercing extends DamageType(2.0) {}

object Impaling extends DamageType(2.0) {}

object Cutting extends DamageType(1.5) {}

