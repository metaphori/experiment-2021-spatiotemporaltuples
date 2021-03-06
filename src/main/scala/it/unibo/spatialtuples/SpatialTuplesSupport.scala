package it.unibo.spatialtuples

import alice.tuprolog.Term
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.space.Point2D
import it.unibo.utils.Utils
import org.scalactic.TripleEquals._
import org.scalactic.Tolerance._


trait SpatialTuplesSupport extends ScafiAlchemistSupport with BlockG with BlockC with BlockS with CustomSpawn with Utils {
  self: AggregateProgram with StandardSensors =>
  import it.unibo.experiments.SpatialTuplesStorm._ // TODO: this is merely for Exports and Effects
  import SpatialTuplesSupport._
  import SpawnInterface._
  import TupleSupport._

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

  // TODO: make OUTs functions uniform
  def OutMeLogic(toid: TupleOpId, outOp: OutMe, arg: ProcArg): (TupleOpResult, Status) = {
    val owner = toid.op.initiator == mid()
    val g = classicGradient(owner)
    val inRegion = g <= outOp.extension
    outResultInRegion(toid, owner, outOp.datum, inRegion, g, arg)
  }

  def OutInRegionLogic(toid: TupleOpId, outOp: OutInRegion, arg: ProcArg): (TupleOpResult, Status) = {
    val owner = gossipBy[ID](mid(), Math.min) == mid()
    //A. toid.op.initiator == mid()
    //B. includingSelf.minHoodSelector(nbr { mid() })(nbr { outOp.region.center.distance(currentPosition()) }) == mid()
    //C. S(Double.PositiveInfinity, nbrRange _)
    val inRegion = outOp.region.withinRegion(currentPosition())
    val g = classicGradient(owner)
    outResultInRegion(toid, owner, outOp.datum, inRegion, g, arg)
  }

  def outResultInRegion(toid: TupleOpId, owner: Boolean, s: Tuple, inRegion: Boolean, potential: Double, arg: ProcArg) = {
    node.put(Effects.DOING_OUT, true)
    if(owner){ inc(Exports.NUM_OUT_INITIATORS) }
    val timeout = branch(owner){
      val lifetime = rep(0L)(_+deltaTime().toMillis)/1000.0
      node.put(Exports.LIFETIME, Math.max(node.get[Double](Exports.LIFETIME).ifNaN(0), lifetime.toDouble))
      lifetime > toid.timeout
    } { false }
    val (events, terminate) = branch(inRegion){ handleRemovalByIN(toid, s, potential, arg) }{ (Set.empty, false) }
    (TupleOpResult(OperationStatus.completed, Some(s), events),
      if(owner && (terminate || timeout)) {
        if(timeout){ extendSet(Molecules.OUTS_TIMEOUT, toid) }
        extendSet(Molecules.OUTS_CLOSED, toid)
        //println(s"[thread=${Thread.currentThread()}][t=$T] node ${mid()} terminating $toid")
        Terminated
      } else if(inRegion) Output else External)
  }

  def handleRemovalByIN(toid: TupleOpId, s: Tuple, potential: Double, arg: ProcArg): (Set[TupleOpEvent], Boolean) = {
    node.put(Effects.LEADER_OUT_PROC, choosePidToShow(node.getOrElse(Effects.LEADER_OUT_PROC, -1), toid.op.initiator))
    val owner = mid() == toid.op.initiator
    if(owner){ node.put(Effects.INITIATOR, true) }
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
          val noINlocally = delay(!arg.keySet.contains(inProc), false, 5)
          val notAliveIN = C[Double, Boolean](potential, _&&_, noINlocally, noINlocally)
          // Wait ack from IN to actually close
          val ack = gossipBy[Boolean](inOwnerAck(toid, inProc, events), _ || _)
          // Close when ack reaches the OUT owner
          val ackEvent: Set[TupleOpEvent] = if(owner && ack) Set(OUTAck(toid)) else Set.empty
          (branch[OutPhase](owner && ack){ delay(OutPhase.Done, thisPhase, 2) } {
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

    if(owner){
      newPhase match {
        case OutPhase.Normal => inc(Exports.NUM_OUTS_PHASE1)
        case OutPhase.Serving(_) => inc(Exports.NUM_OUTS_PHASE2)
        case OutPhase.Done => inc(Exports.NUM_OUTS_PHASE3)
      }
    }
    node.put(Effects.OUT_PHASE, newPhase.toNum)

    (newEvents, newPhase.isDone)
  }

  def ReadLogic(toid: TupleOpId, readOp: Read, arg: ProcArg): (TupleOpResult, Status) = {
    node.put(Effects.DOING_READ, true)
    if(toid.op.initiator == mid()){ node.put(Effects.INITIATOR, true) }
    val Read(tupleTemplate, initiator, extension) = readOp
    val g = classicGradient(initiator==mid())
    // TODO: should exclude OUTs which reserved their tuple to some IN
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
    if(toid.op.initiator == mid()){ node.put(Effects.INITIATOR, true) }
    node.put(Effects.LEADER_IN_PROC, choosePidToShow(node.getOrElse(Effects.LEADER_IN_PROC, -1), toid.op.initiator))
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
        case thisPhase@InPhase.Read(out) => {
          // Just let the owner read the tuple
          if(trueOnFirstExecutionOnly() && owner){ addTupleIfNotAlreadyThere(out.outTuple) }
          // Advertise the tuple that has been removed to everyone in the IN process
          val event = INRemovedTuple(toid, out)
          // Wait ack from OUT to actually close
          val ack = gossipBy[Boolean](outOwnerAck(out, events), _ || _)
          // Close when ack reaches the IN owner
          (branch[InPhase](owner && ack){ delay(InPhase.Done(out), thisPhase, 2) } { InPhase.Read(out) }, Set(event))
        }
        case InPhase.Done(out) => {
          (InPhase.Done(out), Set(INDone(toid)))
        }
      }
    }}

    if(owner){
      newPhase match {
        case InPhase.Start => inc(Exports.NUM_INS_PHASE1)
        case InPhase.Read(_) => inc(Exports.NUM_INS_PHASE2)
        case InPhase.Done(_) => inc(Exports.NUM_INS_PHASE3)
      }
    }
    node.put(if(toid.issuedAtTime<28) Effects.IN_PHASE else Effects.IN_PHASE2, newPhase.toNum)

    val result = TupleOpResult(
      operationStatus = if(newPhase.isDone) OperationStatus.completed else OperationStatus.inProgress,
      result = None,
      newEvents
    )
    val timeout = branch(owner){
      val lifetime = rep(0L)(_+deltaTime().toMillis)/1000.0
      node.put(Exports.LIFETIME, Math.max(node.get[Double](Exports.LIFETIME).ifNaN(0), lifetime.toDouble))
      lifetime > toid.timeout
    } { false }
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

  private def choosePidToShow(a: ID, b: ID): ID = {
    if(Math.abs(a-mid()) < Math.abs(b-mid())) a else b
  }
}

object SpatialTuplesSupport {
  type Tuple = String
  type TupleTemplate = String

  case class SituatedTuple(tuple: Tuple, situation: SpatialSituation)

  case class SituatedTupleTemplate(tupleTemplate: TupleTemplate, situation: SpatialSituation)

  implicit def situateTuple(tuple: Tuple) = SituatedTuple(tuple, Me)

  implicit def situateTupleTemplate(tupleTemplate: TupleTemplate) = SituatedTupleTemplate(tupleTemplate, Me)

  trait Op

  object Op {
    val out: Op = new Op {
      override def toString: String = "out"
    }
    val rd: Op = new Op {
      override def toString: String = "rd"
    }
    val in: Op = new Op {
      override def toString: String = "in"
    }
  }

  trait TupleOp {
    val initiator: ID
  }

  case class OutMe(datum: Tuple, val initiator: ID, val extension: Double = 0) extends TupleOp

  case class OutHere(datum: Tuple, val initiator: ID, val position: Point2D, val extension: Double = 0) extends TupleOp

  case class OutInRegion(datum: Tuple, val initiator: ID, val region: Region) extends TupleOp

  case class Read(template: TupleTemplate, val initiator: ID, val extension: Double = Double.PositiveInfinity) extends TupleOp

  case class In(template: TupleTemplate, val initiator: ID, val extension: Double = Double.PositiveInfinity) extends TupleOp

  case class TupleOpId(uid: String)(val op: TupleOp, val issuedAtTime: Double, val timeout: Double = Double.PositiveInfinity) {
    override def toString: Tuple = uid
    lazy val isOut = op.isInstanceOf[OutMe] || op.isInstanceOf[OutHere] || op.isInstanceOf[OutInRegion]
    lazy val isIn = op.isInstanceOf[In]
    lazy val isRead = op.isInstanceOf[Read]
    lazy val outTuple = if(isOut) {
      op match {
        case OutMe(t, _, _) => t
        case OutHere(t, _, _, _) => t
        case OutInRegion(t, _, _) => t
      }
    } else { throw new Exception("Not an OUT operation ") }
  }

  trait OperationStatus
  object OperationStatus {
    val inProgress: OperationStatus = new OperationStatus { override def toString: Tuple = "IN_PROGRESS" }
    val completed: OperationStatus = new OperationStatus { override def toString: Tuple = "COMPLETED" }
  }

  trait TupleOpEvent
  case class OUTReservedFor(outOp: TupleOpId, forOp: TupleOpId) extends TupleOpEvent
  case class OUTAck(outOp: TupleOpId) extends TupleOpEvent
  case class OUTDone(inOp: TupleOpId) extends TupleOpEvent

  case class INLookingFor(inOp: TupleOpId, ttemplate: TupleTemplate) extends TupleOpEvent
  case class INRemovedTuple(inOp: TupleOpId, outOp: TupleOpId) extends TupleOpEvent
  case class INDone(inOp: TupleOpId) extends TupleOpEvent

  case class TupleOpResult(operationStatus: OperationStatus,
                           result: Option[Tuple],
                           events: Set[TupleOpEvent] = Set.empty)

  //case class ProcArg(localTuples: Set[Tuple] = Set.empty, procs: Set[TupleOpId] = Set.empty)
  type ProcArg = Map[TupleOpId, TupleOpResult]

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
    case class Done(out: TupleOpId) extends InPhase { override val toNum: Int = 3 }
  }
}
