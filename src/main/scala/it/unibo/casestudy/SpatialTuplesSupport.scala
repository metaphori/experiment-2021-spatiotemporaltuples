package it.unibo.casestudy

import alice.tuprolog.Term
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.casestudy.SpatialTuplesSupport.{Tuple, TupleOpId}
import it.unibo.scafi.space.Point2D


trait SpatialTuplesSupport extends ScafiAlchemistSupport with BlockG with BlockC with BlockS with Utils {
  self: AggregateProgram with StandardSensors =>
  import SpatialTuplesSupport._
  import SpawnInterface._
  import TupleSupport._

  // TODO: make OUTs functions uniform
  def OutMeLogic(toid: TupleOpId, outOp: OutMe, arg: ProcArg): (TupleOpResult, Status) = {
    val owner = toid.op.initiator == mid()
    val g = classicGradient(owner)
    val inRegion = g <= outOp.extension
    outResultInRegion(toid, owner, outOp.datum, inRegion, g, arg)
  }

  def OutInRegionLogic(toid: TupleOpId, outOp: OutInRegion, arg: ProcArg): (TupleOpResult, Status) = {
    val owner = S(Double.PositiveInfinity, nbrRange _) // toid.op.initiator == mid()
    val pos = alchemistEnvironment.getPosition(alchemistEnvironment.getNodeByID(outOp.initiator)).getCoordinates
    val inRegion = outOp.region.withinRegion(Point2D(pos(0), pos(1)))
    val g = classicGradient(owner)
    outResultInRegion(toid, owner, outOp.datum, inRegion, g, arg)
  }

  def outResultInRegion(toid: TupleOpId, owner: Boolean, s: Tuple, inRegion: Boolean, potential: Double, arg: ProcArg) = {
    val owner = toid.op.initiator == mid()
    node.put(Effects.DOING_OUT, true)
    node.put(Effects.INITIATOR, owner)
    if(owner){ inc(Exports.NUM_OUT_INITIATORS) }
    val timeout = branch(owner){ rep(0L)(_+deltaTime().toMillis)/1000.0 > toid.timeout } { false }
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
    case class Done(outTuple: Tuple) extends InPhase { override val toNum: Int = 3 }
  }
}
