package CombatSim.Messages

import CombatSim.Fighter.Fighter
/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 7:35 PM
 * To change this template use File | Settings | File Templates.
 */

sealed trait SimMessage

case object Simulate extends SimMessage

case class SimulateCombat(fighters: Array[Fighter], nrOfSimulations: Int) extends SimMessage

case class CombatResult(result: List[Double]) extends SimMessage