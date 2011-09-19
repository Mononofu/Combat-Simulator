package CombatSim.Tools

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 7:31 PM
 * To change this template use File | Settings | File Templates.
 */

case class Dice(numDice: Int = 3, mod: Int = 0) {
  def roll() = {
    mod + (for (i <- 0 until numDice) yield util.Random.nextInt(6) + 1).sum
  }
}

object DefaultDice extends Dice {
  def check(n: Int) = {
    if (roll() <= n) true else false
  }
}
