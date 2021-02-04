package it.unibo.simulations

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.space.{Point2D, Point3D}
import it.unibo.utils.MovementUtils

class TestAggregateProcesses extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with CustomSpawn with Gradients with MovementUtils {
  import SpawnInterface._

  /**
   * This program realises simple spawning of gradient processes.
   * The "difficult" part lies in controlling the processes' lifecycle.
   */
  override def main(): Any = {

    val t = alchemistTimestamp.toDouble.toLong
    val procs = node.get[Map[Int,(Int,Int)]]("procs")
    val pids: Set[Pid] = procs.filter(tgen => tgen._2._1 == mid() && t > tgen._1 && (t - 5) < tgen._1)
      .map(tgen => Pid(time = tgen._1)(terminateAt = tgen._2._2)).toSet

    val maps = sspawn[Pid,Unit,Double](process, pids, {})

    if(!maps.isEmpty) {
      node.put("pid", Math.abs(maps.maxBy(_._1.time)._1.hashCode()) % 100)
      node.put("g", maps.maxBy(_._1.time)._2)
    } else { removeMolecule("pid"); removeMolecule("g"); }

    rectangleWalk()
    node.put("pids", maps.keySet)
    node.put("numPids", maps.size)
  }

  // TODO: fix remove to perform the check
  def removeMolecule(name: String) = if(node.has(name)) node.remove(name)

  case class Pid(src: ID = mid(), time: Long = alchemistTimestamp.toDouble.toLong)
                (val terminateAt: Long = Long.MaxValue)

  def process(pid: Pid)(src: Unit={}): POut[Double] = {
    val g = classicGradient(pid.src==mid())
    val s = if(pid.src==mid() && pid.terminateAt.toDouble <= alchemistTimestamp.toDouble){
      Terminated
    } else if(g < 200) Output else External
    POut(g, s)
  }

  /*
  override def sspawn[A, B, C](process: A => B => POut[C], params: Set[A], args: B): Map[A,C] = {
    spawn[A,B,Option[C]]((p: A) => (a: B) => {
      val firstTime = rep(0)(_ + 1) == 1
      val (finished, result, status) = rep((false, none[C], false)) { case (finished, _, _) => {
        val POut(result, status) = process(p)(a)
        val newFinished = status == Terminated | includingSelf.anyHood(nbr{finished})
        if(newFinished){ println(s"${mid()} finishing") }
        val terminated = (firstTime && newFinished) | includingSelf.everyHood(nbr{newFinished})
        if(newFinished){ println(s"${mid()} terminating as everybody else has finished") }
        val (newResult, newStatus) = (result, status) match {
          case _ if terminated     => (None, false)
          case (_,     External)   => (None, false)
          case (_,     Terminated) => (None, true)
          case (value, Output)     => (Some(value), true)
          case (_,     Bubble)     => (None, true)
        }
        (newFinished, newResult, newStatus)
      } }
      //val exitTuple = s"""exit("${p}")"""
      //if(!status){ addTupleIfNotAlreadyThere(exitTuple) } else { removeTuple(exitTuple) }
      (result, status)
    }, params, args).collect { case (k, Some(p)) => k -> p }
  }
  def none[T]: Option[T] = None

   */
}