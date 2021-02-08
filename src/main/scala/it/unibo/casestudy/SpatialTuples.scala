package it.unibo.casestudy

import alice.tuprolog.{Prolog, Term}
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.casestudy.SpatialTuplesSupport._
import it.unibo.scafi.space.Point2D
import it.unibo.utils.MovementUtils
import org.apache.commons.math3.distribution.ExponentialDistribution
import org.scalactic.Tolerance._
import org.scalactic.TripleEquals._

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
}
object Molecules {
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

trait OutPhase {
  val toNum: Int
  def isDone: Boolean = this match { case OutPhase.Done => true; case _ => false }
}
object OutPhase {
  case object Normal extends OutPhase { override val toNum: Int = 1 }
  case class Serving(out: TupleOpId) extends OutPhase { override val toNum: Int = 2 }
  case object Done extends OutPhase { override val toNum: Int = 3 }
}

trait InPhase {
  val toNum: Int
  def isDone: Boolean = this match { case InPhase.Done(_) => true; case _ => false }
}
object InPhase {
  case object Start extends InPhase { override val toNum: Int = 1 }
  case class Read(out: TupleOpId) extends InPhase { override val toNum: Int = 2 }
  case class Done(outTuple: Tuple) extends InPhase { override val toNum: Int = 3 }
}

class SpatialTuples extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with CustomSpawn with Gradients with MovementUtils with BlockC with BlockG with TupleSpace
  with Utils
  /*with LindaDSL*/ {
  import SpawnInterface._

  lazy val expDistibution = new ExponentialDistribution(alchemistRandomGen, 1.0)

  def T = alchemistTimestamp.toDouble.toLong

  var opsStarted: Set[TupleOpId] = Set.empty
  var opsClosed: Set[TupleOpId] = Set.empty

  def initialise() = {
    initialiseExports()
    initialiseEffects()
  }

  def initialiseExports() = {
    node.put(Exports.RUNNING_PROCESSES, 0)
    node.put(Exports.NUM_IN_INITIATORS, 0)
    node.put(Exports.NUM_OUT_INITIATORS, 0)
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
    val ops: Map[TupleOpId,TupleOpResult] = sspawn[TupleOpId,Map[TupleOpId,TupleOpResult],TupleOpResult](tupleOperation _, pids, node.getOrElse("tuple_ops", Map.empty))

    // Log stuff
    //node.put("local-tuple-space", tupleSpace.getTheory.getText)
    node.put("tuple_ops", ops)
    node.put(Exports.NUM_OUTS_CLOSED, node.getOrElse[Set[TupleOpId]](Molecules.OUTS_CLOSED, Set.empty).size)
    node.put(Exports.NUM_INS_CLOSED, node.getOrElse[Set[TupleOpId]](Molecules.INS_CLOSED, Set.empty).size)
    node.put(Exports.NUM_OUTS_TIMEOUT, node.getOrElse[Set[TupleOpId]](Molecules.OUTS_TIMEOUT, Set.empty).size)
    node.put(Exports.NUM_INS_TIMEOUT, node.getOrElse[Set[TupleOpId]](Molecules.INS_TIMEOUT, Set.empty).size)
  }

  def tupleOperation(toid: TupleOpId)(arg: ProcArg): (TupleOpResult, Status) = {
    inc(Exports.RUNNING_PROCESSES)
    val firstTime = trueOnFirstExecutionOnly()
    branch(toid.op.initiator==mid() && firstTime && !(toid.issuedAtTime === alchemistTimestamp.toDouble +- 0.1)){ // prevent reentrance
      // println("RE-ENTRANCE ATTEMPT!")
      (TupleOpResult(OperationStatus.completed,None), External)
    } {
      val res = toid.op match {
        case outop@OutMe(s, initiator, extension) => OutMeLogic(toid, outop, arg)
        case outop@OutInRegion(s, initiator, region) => OutInRegionLogic(toid, outop, arg)
        case outop@OutHere(s, initiator, position, extension) => OutInRegionLogic(toid, OutInRegion(s, initiator, CircularRegion(position, extension)), arg)
        case readop@Read(ttemplate, initiator, extension) => ReadLogic(toid, readop, arg)
        case inop@In(ttemplate, initiator, extension) => InLogic(toid, inop, arg)
        case _ => ??? // (OperationResult("invalid"), Terminated)
      }

      val status = res._2
      //node.put(toid.uid + "_op", s"${toid.op}{$T}")
      //node.put(toid.uid + "_status", (if (status == Output) 2 else if (status == Bubble) 1 else if (status == Terminated) 3 else 0) + s" {$T}")
      res
    }
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
        alchemistTimestamp.toDouble, 200))
    } else { Set.empty }
    val ins = if(T > iFrom && T < iTo && expDistibution.sample() > iTh){
      inc(Exports.NUM_INS)
      //println(s"[thread=${Thread.currentThread()}][t=$T] node ${mid()} generating an IN")
      Set(TupleOpId(s"${T}_${mid()}_in")(In(s"${templates(((templates.size-1)*nextRandom()).round.toInt)}(X)", mid(), 500.0), alchemistTimestamp.toDouble, 200))
    } else { Set.empty }
    outs ++ ins
  }

  // TODO: make OUTs functions uniform
  def OutMeLogic(toid: TupleOpId, outOp: OutMe, arg: ProcArg): (TupleOpResult, Status) = {
    val g = classicGradient(toid.op.initiator == mid())
    val inRegion = g <= outOp.extension
    outResultInRegion(toid, outOp.datum, inRegion, g, arg)
  }

  def OutInRegionLogic(toid: TupleOpId, outOp: OutInRegion, arg: ProcArg): (TupleOpResult, Status) = {
    val pos = alchemistEnvironment.getPosition(alchemistEnvironment.getNodeByID(outOp.initiator)).getCoordinates
    val inRegion = outOp.region.withinRegion(Point2D(pos(0), pos(1)))
    val g = classicGradient(toid.op.initiator == mid())
    outResultInRegion(toid, outOp.datum, inRegion, g, arg)
  }

  def outResultInRegion(toid: TupleOpId, s: Tuple, inRegion: Boolean, potential: Double, arg: ProcArg) = {
    val owner = toid.op.initiator == mid()
    node.put(Effects.DOING_OUT, true)
    node.put(Effects.INITIATOR, owner)
    if(owner){ inc(Exports.NUM_OUT_INITIATORS) }
    val timeout = branch(owner){ rep(0L)(_+deltaTime().toMillis)/1000.0 > toid.timeout } { false }
    val (events, terminate) = branch(inRegion){ handleRemovalByIN(toid, s, potential, arg) }{ (Set.empty, false) }
    (TupleOpResult(OperationStatus.completed, Some(s), events),
      if(mid()==toid.op.initiator && (terminate || timeout)) {
        if(timeout){ extendSet(Molecules.OUTS_TIMEOUT, toid) }
        extendSet(Molecules.OUTS_CLOSED, toid)
        //println(s"[thread=${Thread.currentThread()}][t=$T] node ${mid()} terminating $toid")
        Terminated
      } else if(inRegion) Output else External)
  }

  def handleRemovalByIN(toid: TupleOpId, s: Tuple, potential: Double, arg: ProcArg): (Set[TupleOpEvent], Boolean) = {
    val owner = toid.op.initiator == toid.op.initiator
    val events = arg.flatMap(_._2.events).toSet

    val (newPhase,newEvents) = rep[(OutPhase,Set[TupleOpEvent])]((OutPhase.Normal, Set.empty)) { case (currPhase, _) => {
      val phase: OutPhase = broadcastOn(potential, currPhase)

      branchOn(phase) {
        case OutPhase.Normal => {
          // Look for local INs
          val localMatches: Set[TupleOpId] = findINsForThisOUT(s, arg.keySet, events)
          // Commit to one matching IN for a while
          val localOut = keepUntil[Option[TupleOpId]](localMatches.headOption, until = prev => prev.isEmpty || !localMatches.contains(prev.get))
          // Collect proposals to owner
          val potentialMatches: Set[TupleOpId] = C[Double, Set[TupleOpId]](potential, _++_, localOut.toSet, Set.empty)
          // Let the owner choose one nondeterministically
          val chosenOutProcess: Option[TupleOpId] = potentialMatches.headOption.filter(_ => owner)
          // Move to another phase if a choice has been made
          (if(chosenOutProcess.isEmpty) OutPhase.Normal else OutPhase.Serving(chosenOutProcess.get), Set.empty)
        }
        case thisPhase@OutPhase.Serving(inProc) => {
          // TODO: fix the need for that delay
          val notAliveIN = C[Double, Boolean](potential, _&&_, delay(!arg.keySet.contains(inProc), false, 5), true)
          // Wait ack from IN to actually close
          val ack = C[Double, Boolean](potential, _||_, inOwnerAck(toid, inProc, events), false)
          // Close when ack reaches the OUT owner
          val ackEvent: Set[TupleOpEvent] = if(owner && ack) Set(OUTAck(toid)) else Set.empty
          (branch[OutPhase](owner && ack){ delay(OutPhase.Done, thisPhase) } {
            // If the IN is not alive anymore, back to normal phase
            branch[OutPhase](owner && !notAliveIN){ thisPhase }{ OutPhase.Normal }
          }, Set(OUTReservedFor(toid, inProc))++ackEvent)
        }
        case OutPhase.Done => {
          (OutPhase.Done, Set(OUTAck(toid), OUTDone(toid)))
        }
        case ph => throw new Exception(s"Invalid phase ${ph}")
      }
    }}

    node.put(Effects.OUT_PHASE, newPhase.toNum)

    (newEvents, newPhase.isDone)
  }

  def ReadLogic(toid: TupleOpId, readOp: Read, arg: ProcArg): (TupleOpResult, Status) = {
    node.put(Effects.DOING_READ, true)
    node.put(Effects.INITIATOR, toid.op.initiator == mid())
    val Read(tupleTemplate, initiator, extension) = readOp
    val g = classicGradient(initiator==mid())
    val tupleFound: Set[Tuple] = arg.keySet.filter(_.isOut).map(_.outTuple).filter(t => tupleSpace.unify(tupleTemplate.toTerm(), t.toTerm()))
    val result  = C[Double,Set[Tuple]](g, _++_, tupleFound, Set.empty)
    val requesterGotResult = mid()==initiator && !result.isEmpty
    val chosenTuple: Option[Tuple] = branch(requesterGotResult){ keepUntil[Option[Tuple]](result.headOption, until = t => t.isEmpty) } { None }
    if(chosenTuple.isDefined){ addTupleIfNotAlreadyThere(chosenTuple.get) }
    val status = if(chosenTuple.isDefined){ Terminated } else if(g < extension) { Output } else { External }
    (TupleOpResult(if(chosenTuple.isDefined) OperationStatus.completed else OperationStatus.inProgress, chosenTuple), status)
  }

  def InLogic(toid: TupleOpId, inOp: In, arg: ProcArg): (TupleOpResult, Status) = {
    node.put(Effects.DOING_IN, true)
    node.put(Effects.INITIATOR, toid.op.initiator == mid())
    if(toid.op.initiator == mid()){ inc(Exports.NUM_IN_INITIATORS) }
    val In(ttemplate, initiator, extension) = inOp
    val owner = mid()==initiator
    val events = arg.flatMap(_._2.events).toSet
    // Note: IN is not trivial: Need consensus on the tuple to remove (as there might be multiple OUTs);
    // As there might be multiple concurrent INs, these must be discriminated by a tuple's owner.
    val g = classicGradient(owner)

    val (newPhase,newEvents) = rep[(InPhase,Set[TupleOpEvent])]((InPhase.Start, Set.empty)){ case (currPhase, _) => {
      val phase = broadcastOn(g, currPhase)

      branchOn(phase){
        case InPhase.Start => {
          // Look for local matching OUTs
          val localMatches: Set[TupleOpId] = findOUTsForThisIN(toid, events)
          // Commit to one matching OUT for a while
          val localOut: Option[TupleOpId] = keepUntil[Option[TupleOpId]](localMatches.headOption, until = prev => prev.isEmpty || !localMatches.contains(prev.get))
          // Collect proposals to owner
          val potentialMatches: Set[TupleOpId] = C[Double,Set[TupleOpId]](g, _++_, localOut.toSet, Set.empty)
          // Let the owner choose one nondeterministically
          val chosenOutProcess: Option[TupleOpId] = potentialMatches.headOption.filter(_ => owner)
          // Move to another phase if a choice has been made
          (if(chosenOutProcess.isEmpty) InPhase.Start else InPhase.Read(chosenOutProcess.get),
            Set(INLookingFor(toid, ttemplate)) // Advertise with an event this IN process wants a tuple matching ttemplate
          )
        }
        case InPhase.Read(out) => {
          // Just let the owner read the tuple
          if(trueOnFirstExecutionOnly() && owner){ addTupleIfNotAlreadyThere(out.outTuple) }
          // Advertise the tuple that has been removed to everyone in the IN process
          val event = INRemovedTuple(toid, out)
          // Wait ack from OUT to actually close
          val ack = C[Double, Boolean](g, _||_, keepUntil[Boolean](outOwnerAck(out, events), until = v => !v), false)
          // Close when ack reaches the IN owner
          (if(owner && ack){ InPhase.Done(out.outTuple) } else { InPhase.Read(out) }, Set(event))
        }
        case InPhase.Done(outTuple) => {
          (InPhase.Done(outTuple), Set(INDone(toid)))
        }
      }
    }}

    node.put(if(toid.issuedAtTime<28) Effects.IN_PHASE else Effects.IN_PHASE2, newPhase.toNum)

    val result = TupleOpResult(
      operationStatus = if(newPhase.isDone) OperationStatus.completed else OperationStatus.inProgress,
      result = None,
      newEvents
    )
    val timeout = branch(owner){ rep(0L)(_+deltaTime().toMillis)/1000.0 > toid.timeout } { false }
    if(timeout){ extendSet(Molecules.INS_TIMEOUT, toid) }
    val status = if(mid()==initiator && (newPhase.isDone || timeout)) {
      extendSet(Molecules.INS_CLOSED, toid)
      // println(s"[thread=${Thread.currentThread()}][t=$T] node ${mid()} terminating $toid")
      Terminated
    } else if(g < extension){ Output } else { External }
    (result, status)
  }

  def findINsForThisOUT(tuple: Tuple, tupleProcesses: Set[TupleOpId], events: Set[TupleOpEvent]): Set[TupleOpId] =
    tupleProcesses
      .filter(potentialIN => !events.contains(INDone(potentialIN)))
      .map(toid => (toid,toid.op)).collect {
      case (op, In(ttemplate, _, _)) if tupleSpace.unify(Term.createTerm(tuple), Term.createTerm(ttemplate)) => op
    }

  def inOwnerAck(toid: TupleOpId, inproc: TupleOpId, events: Set[TupleOpEvent]): Boolean =
    events.collectFirst {
      case INRemovedTuple(`inproc`, `toid`) => true
    }.isDefined

  def findOUTsForThisIN(toid: TupleOpId, events: Set[TupleOpEvent]): Set[TupleOpId] =
    events
      .collect {  case OUTReservedFor(from, `toid`) => from}
      // Need to filter out OUT processes that already committed to other INs
      .filter(potentialOut => events.exists { case OUTReservedFor(`potentialOut`, `toid`) => true; case _ => false })

  def outOwnerAck(outP: TupleOpId, events: Set[TupleOpEvent]): Boolean =
    events.collectFirst {
      case OUTAck(`outP`) => true
    }.isDefined
}