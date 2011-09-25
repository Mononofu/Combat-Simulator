package CombatSim.Sim


import akka.actor.Actor.actorOf
import java.util.concurrent.CountDownLatch
import CombatSim.WorkMaster.WorkMaster
import CombatSim.Messages.Simulate
import CombatSim.CharacterSheet._

object Interface extends App {

  val latch = new CountDownLatch(1)

  val strongBoy = CharacterSheet("Hulk", ST -> 15, HT -> 12, HP -> 12, WeaponSkill -> 12, Parry -> 9)
  val quickOne = CharacterSheet("The Flash", ST -> 11, HT -> 11, BS -> 6, Dodge -> 10, WeaponSkill -> 15, Parry -> 12)
  val loser = CharacterSheet("Loser", ST -> 9, HT -> 10, BS -> 4, Dodge -> 8, WeaponSkill -> 11, Parry -> 8)


  // create the master
  val master = actorOf(new WorkMaster(8, 24, 12000, latch, strongBoy, quickOne, loser)).start()

  // start the calculation
  master ! Simulate

  // wait for master to shut down
  latch.await()
}
