package CombatSim.Messages

import CombatSim.Fighter.Fighter
import CombatSim.AI.AI
/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 9/19/11
 * Time: 7:35 PM
 * To change this template use File | Settings | File Templates.
 */

sealed trait SimMessage

case object Simulate extends SimMessage

case class SimulateCombat(fighters: Array[Fighter], AIs: Array[AI], nrOfSimulations: Int) extends SimMessage

case class CombatResult(fighterStats: Map[String, Int], AIStats: Map[Int, Int]) extends SimMessage