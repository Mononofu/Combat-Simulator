package CombatSim.HitLocation

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 8:31 PM
 * To change this template use File | Settings | File Templates.
 */

import CombatSim.DamageType.Damage

abstract class HitLocation {
  // dmg is (actual damage value, injury multiplier)
  def calcDamage(dmg: Damage) = dmg
}

class Untargeted extends HitLocation {}

class Skull extends HitLocation {
  override def calcDamage(dmg: Damage) = Damage(dmg.baseDamage - 2, 4)
}