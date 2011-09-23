package CombatSim.Tools

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 7:31 PM
 * To change this template use File | Settings | File Templates.
 */

sealed trait ResultType

case object CriticalSuccess extends ResultType

case object Success extends ResultType

case object Failure extends ResultType

case object CriticalFailure extends ResultType


case class Dice(numDice: Int = 3, mod: Int = 0) {
  def roll() = {
    mod + (for (i <- 0 until numDice) yield util.Random.nextInt(6) + 1).sum
  }

  def resultType(roll: Int, target: Int) = {
    roll match {
      case 3 | 4 => CriticalSuccess
      case 18 => CriticalFailure
      case r if r <= target =>
        r match {
          case 5 => if (target >= 15) CriticalSuccess else Success
          case 6 => if (target >= 16) CriticalSuccess else Success
          case _ => Success
        }
      case _ =>
        if (roll >= target + 10) CriticalFailure
        else Failure
    }
  }
}

object DefaultDice extends Dice {
  def check(n: Int) = resultType(roll(), n)
}
