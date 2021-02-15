package it.unibo.experiments

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.spatialtuples.SpatialTuplesSupport
import it.unibo.spatialtuples.SpatialTuplesSupport._
import it.unibo.utils.MovementUtils
import org.apache.commons.math3.distribution.ExponentialDistribution


class SpatialTuplesStorm extends AggregateProgram with StandardSensors with CustomSpawn with Gradients with MovementUtils with SpatialTuplesSupport
  /*with LindaDSL*/ {
  import SpatialTuplesStorm._

  lazy val expDistibution = new ExponentialDistribution(alchemistRandomGen, 1.0)

  def initialise() = {
    initialiseExports()
    initialiseEffects()
  }

  def initialiseExports() = {
    node.put(Exports.RUNNING_PROCESSES, 0)
    node.put(Exports.NUM_IN_INITIATORS, 0)
    node.put(Exports.NUM_OUT_INITIATORS, 0)
    node.put(Exports.NUM_INS_PHASE1, 0)
    node.put(Exports.NUM_INS_PHASE2, 0)
    node.put(Exports.NUM_INS_PHASE3, 0)
    node.put(Exports.NUM_OUTS_PHASE1, 0)
    node.put(Exports.NUM_OUTS_PHASE2, 0)
    node.put(Exports.NUM_OUTS_PHASE3, 0)
  }

  def initialiseEffects() = {
    node.put(Effects.DOING_IN, false)
    node.put(Effects.DOING_OUT, false)
    node.put(Effects.DOING_READ, false)
    node.put(Effects.IN_PHASE, 0)
    node.put(Effects.IN_PHASE2, 0)
    node.put(Effects.OUT_PHASE, 0)
    node.put(Effects.INITIATOR, false)
  }

  override def main(): Any = {
    initialise()

    val pids: Set[TupleOpId] = stormScenaro() //basicScenario()
    // Spawn processes and get results
    rep(Map[TupleOpId,TupleOpResult]())(ops =>
      sspawn[TupleOpId,Map[TupleOpId,TupleOpResult],TupleOpResult](tupleOperation _, pids, ops)
    )

    rectangleWalk()

    // Log stuff
    //node.put("local-tuple-space", tupleSpace.getTheory.getText)
    //node.put("tuple_ops", ops)
    node.put(Exports.NUM_OUTS_CLOSED, node.getOrElse[Set[TupleOpId]](Molecules.OUTS_CLOSED, Set.empty).size)
    node.put(Exports.NUM_INS_CLOSED, node.getOrElse[Set[TupleOpId]](Molecules.INS_CLOSED, Set.empty).size)
    node.put(Exports.NUM_OUTS_TIMEOUT, node.getOrElse[Set[TupleOpId]](Molecules.OUTS_TIMEOUT, Set.empty).size)
    node.put(Exports.NUM_INS_TIMEOUT, node.getOrElse[Set[TupleOpId]](Molecules.INS_TIMEOUT, Set.empty).size)
  }

  /**
   * Basic scenario with 4 concurrent and overlapping operations: 2 OUTs and 2 INs
   */
  def basicScenario(): Set[TupleOpId] = {
    val (topleft, top, left, center, right, bottomleft) = (380, 390, 180, 190, 199, 0)
    val procs = Map(0 ->  (bottomleft,   50, "out", "x(77)"),
      30 -> (top, 50,  "in", "x(X)"),
      25 -> (center, 35,  "in", "x(X)"),
      30 -> (left, 100, "rd", "x(X)"),
      50 -> (right, 1, "rd", "x(X)"),
      20 -> (topleft, 85, "out", "x(88)")
    ) //node.get[Map[Int,(Int,Int,String,String)]](Molecules.PROCS) // format T -> (ID, duration, "in"/"out", tuple)
    val pids: Set[TupleOpId] = procs.filter { case (t, (src,duration,kind,tpl)) => src == mid() && T > t && (T - 5) < t }
      .map{ case (t, (src,duration,kind,tpl)) => kind match {
        case "out" => TupleOpId(s"${mid()}_${t}")(OutMe(tpl, src, 400), t)
        case "in" => TupleOpId(s"${mid()}_${t}")(In(tpl, src, 400), t)
        case "rd" => TupleOpId(s"${mid()}_${t}")(Read(tpl, src, 100), t)
      }}.toSet
    pids
  }

  def stormScenaro(): Set[TupleOpId] = {
    val maxExt = node.get[Double](Molecules.MAX_EXTENSION)
    val (oFrom,oTo) = node.get[(Double,Double)](Molecules.OUT_WINDOW)
    val (iFrom,iTo) = node.get[(Double,Double)](Molecules.IN_WINDOW)
    val (oTh, iTh) = (node.get[Double](Molecules.OUT_EXP_THRES), node.get[Double](Molecules.OUT_EXP_THRES))
    val templates = Array("a") // ,"b","c")
    val outs = if(T > oFrom && T < oTo && expDistibution.sample() > oTh){
      inc(Exports.NUM_OUTS)
      //println(s"[t=$T] node ${mid()} generating an OUT")
      Set(TupleOpId(s"${T}_${mid()}_out")(OutMe(s"${templates(((templates.size-1)*nextRandom()).round.toInt)}(${(nextRandom()*100).round})", mid(), 500.0),
        alchemistTimestamp.toDouble, node.getOrElse(Molecules.OP_TIMEOUT, 200)))
    } else { Set.empty }
    val ins = if(T > iFrom && T < iTo && expDistibution.sample() > iTh){
      inc(Exports.NUM_INS)
      //println(s"[thread=${Thread.currentThread()}][t=$T] node ${mid()} generating an IN")
      Set(TupleOpId(s"${T}_${mid()}_in")(In(s"${templates(((templates.size-1)*nextRandom()).round.toInt)}(X)", mid(), 500.0),
        alchemistTimestamp.toDouble, node.getOrElse(Molecules.OP_TIMEOUT, 200)))
    } else { Set.empty }
    outs ++ ins
  }
}

object SpatialTuplesStorm {
  object Exports {
    val NUM_OUT_INITIATORS = "outs_devs_n"
    val NUM_IN_INITIATORS = "ins_devs_n"
    val NUM_OUTS = "outs_n"
    val NUM_INS = "ins_n"
    val NUM_OUTS_CLOSED = "outs_closed_n"
    val NUM_INS_CLOSED = "ins_unblocked_n"
    val RUNNING_PROCESSES = "running_processes"
    val NUM_OUTS_TIMEOUT = "outs_timeout_n"
    val NUM_INS_TIMEOUT = "ins_timeout_n"
    val NUM_OUTS_PHASE1 = "outs_phase1"
    val NUM_OUTS_PHASE2 = "outs_phase2"
    val NUM_OUTS_PHASE3 = "outs_phase3"
    val NUM_INS_PHASE1 = "ins_phase1"
    val NUM_INS_PHASE2 = "ins_phase2"
    val NUM_INS_PHASE3 = "ins_phase3"
  }
  object Molecules {
    val OP_TIMEOUT: String = "opTimeout"
    val MAX_EXTENSION: String = "maxExtension"
    val PROCS = "procs"
    val OUT_WINDOW = "outWindow"
    val OUT_EXP_THRES = "outExpThreshold"
    val IN_WINDOW = "inWindow"
    val IN_EXP_THRES = "inExpThreshold"
    val OUTS_CLOSED = "closed_outs"
    val INS_CLOSED = "closed_ins"
    val INS_TIMEOUT: String = "timeout_ins"
    val OUTS_TIMEOUT: String = "timeout_outs"
  }
  object Effects {
    val OUT_PHASE = "out_phase"
    val IN_PHASE = "in_phase"
    val IN_PHASE2 = "in_phase2"
    val DOING_OUT = "doing_out"
    val DOING_IN = "doing_in"
    val DOING_READ = "doing_read"
    val INITIATOR = "initiator"
  }
}