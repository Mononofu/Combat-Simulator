package CombatSim.DamageType

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 8:34 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class DamageType(mul: Double) {
  def calcDamage(dmg: Int) = if(dmg >= 1) math.max((dmg * mul).toInt, 1) else 0
}

class SmallPiercing extends DamageType(0.5) {}

class Piercing extends DamageType(1.0) {}

class LargePiercing extends DamageType(1.5) {}

class Impaling extends DamageType(2.0) {}

class Cutting extends DamageType(1.5) {}

