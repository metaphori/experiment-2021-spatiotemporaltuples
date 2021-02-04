package it.unibo.casestudy

import alice.tuprolog.{Prolog, Term}
import it.unibo.alchemist.model.implementations.nodes.NodeManager
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.casestudy.SpatialTuplesSupport._
import it.unibo.casestudy.TupleSupport._
import it.unibo.scafi.space.Point2D
import it.unibo.utils.MovementUtils
import org.apache.commons.math3.distribution.ExponentialDistribution

object Exports {
  val NUM_OUTS = "outs_n"
  val NUM_INS = "ins_n"
  val NUM_OUTS_CLOSED = "outs_closed_n"
  val NUM_INS_CLOSED = "ins_unblocked_n"
}

object Molecules {

}

class SpatialTuples extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with CustomSpawn with Gradients with MovementUtils with BlockC with BlockG with TupleSpace {
  import SpawnInterface._

  lazy val expDistibution = new ExponentialDistribution(alchemistRandomGen, 1.0)
  var k = 0
  val linda = new LindaDSL

  def T = alchemistTimestamp.toDouble.toLong

  var opsStarted: Set[TupleOpId] = Set.empty
  var opsClosed: Set[TupleOpId] = Set.empty

  override def main(): Any = {
    k = rep(0)(_+1)

    val taskGenerator = expDistibution.sample() > 6.2 && T < 100
    val taskStealer = expDistibution.sample() > 6.5 && T < 150
    val r = nextRandom()

    val newOut = if(taskGenerator) {
      inc(Exports.NUM_OUTS)
      val outOp = TupleOpId(s"out_${mid()}_$T")(OutMe(Task(s"${mid()},$T"), mid(), 400*r))
      Some(outOp)
    } else { None }
    val newIn = if(taskStealer) {
      inc(Exports.NUM_INS)
      val inOp = TupleOpId(s"in_${mid()}_$T")(In("task(X,Y)", mid(), 300*r))
      Some(inOp)
    } else { None }

    val outs: Set[TupleOpId] = newOut.toSet
    val ins: Set[TupleOpId] = newIn.toSet
    val newOps = outs ++ ins

    val ops: Map[TupleOpId,OperationResult] = sspawn(tupleOperation _, newOps, ProcArg())

    val opsStarted = rep[Set[TupleOpId]](Set.empty)(s => s++newOps)
    val opsClosed = rep[Set[TupleOpId]](Set.empty)(closed => {
      closed ++ (opsStarted -- closed).filter(op => {
        val wip = ops.contains(op)
        //println(s"${mid()} :: Consider $op -- wip? $wip -- opsStarted $opsStarted -- opsClosed closed")
        (!wip && (op.op match {
            case OutMe(_, _, _) => {
              //println("OUT closed");
              inc(Exports.NUM_OUTS_CLOSED);
              true
            }
            case _ => false
          })) ||
          (wip && ops(op).operationStatus==OperationStatus.completed && (op.op match {
            case In(_,_,_) => { /*println("IN closed");*/ inc(Exports.NUM_INS_CLOSED); true }
            case _ => false
          }
          ))
        })
    })

    /*
    val task: Option[String] = solveWithMatch("currentTask(T)", "T")
    node.put("task", task)
    val taskGenerator = mid == 23 || mid == 114 || mid == 127
    val taskStealer = mid == 84 || mid == 94 // mid % 27 == 0
    val taskReader = false // mid % 5 == 0

    // val doGenerateTask = taskGenerator && !taskProposal.isEmpty
    node.put("doGenerateTask", taskProposal.isDefined /* doGenerateTask */)
    val doReadTask = alchemistTimestamp.toDouble < 150 && alchemistRandomGen.nextGaussian()>2.75 // (taskStealer || taskReader) && task.isEmpty
    node.put("doReadTask", taskReader && doReadTask)
    node.put("doStealTask", taskStealer && doReadTask)
     */

    /*
    import linda._

    process(taskGenerator){
      when(taskProposal){ t => out(s"task(${t(0)})" @@ AroundMe(t(1).toDouble)) }.andNext((tuple: Tuple) => {
        removeTuple(s"taskToGenerate($tuple)")
      })
    }

    process(taskStealer){
      when(doReadTask) { in("task(X)" @@@ AroundMe(50)) }.andNext((tuple: Tuple)  => {
        node.extendSetWith("ins_unblocked", tuple)
      })
    }
     */

    /*
    node.put("tasks", taskProposal.map(_=>1).getOrElse(0))
    taskProposal.foreach(t => {
        node.put("outCount", node.getOption[Int]("outCount").getOrElse(0)+1)
        removeTuple(Task(t.solution)(false).toString())
        addTuple(Task(t.bindings("X"))(true).toString)
    })
     */

  }

  def inc(name: String) = node.put[Int](name, node.getOrElse(name, 0)+1)

  /*
  def initialiseTasks() = {
    import org.apache.commons.math3.distribution.ExponentialDistribution
    val r = new ExponentialDistribution(alchemistRandomGen, 1.0)
    if(alchemistTimestamp.toDouble < 100 & r.sample() > 5.5) {
      val v = "task" + mid() + "y" + alchemistTimestamp.toDouble.toLong
      //node.put("task-log", node.getOption[String]("task-log").getOrElse("") + "; " + v)
      addTuple(Task(v)().toString())
    }
    if(alchemistTimestamp.toDouble < 150 & r.sample() > 6.0) {
      //node.put("task-log", node.getOption[String]("task-log").getOrElse("") + "; " + in)
      addTuple(Task(v)().toString())
    }
  }
   */

  case class Task(name: String) {
    override def toString: Tuple = s"task($name)"
  }
  object Task {
    implicit def toStr(t: Task): String = t.toString
  }

  case class SituatedTuple(tuple: Tuple, situation: SpatialSituation)
  case class SituatedTupleTemplate(tupleTemplate: TupleTemplate, situation: SpatialSituation)

  implicit def situateTuple(tuple: Tuple) = SituatedTuple(tuple, Me)
  implicit def situateTupleTemplate(tupleTemplate: TupleTemplate) = SituatedTupleTemplate(tupleTemplate, Me)

  class LindaDSL {
    var enabled = false
    def process(condition: => Boolean)(expr: => Any) = {
      enabled = condition
      val res = expr
      enabled = false
      res
    }

    def when(condition: => Boolean)(expr: => TupleOpId): Map[TupleOpId,OperationResult] = {
      val c = enabled && condition
      val toid: Set[TupleOpId] = if(goesUp(c)) Set(expr) else Set.empty
      sspawn(tupleOperation _, toid, ProcArg())
    }

    def when[T](gen: => Option[T])(expr: T => TupleOpId): Map[TupleOpId,OperationResult] = {
      val toid: Option[TupleOpId] = gen.filter(_ => enabled).map(expr)
      sspawn(tupleOperation _, toid.toSet, ProcArg())
    }

    def out(tuple: SituatedTuple): TupleOpId = TupleOpId(s"${mid()}_out_${tuple.hashCode()}")(tuple.situation match {
      case AroundMe(ext) => OutMe(tuple.tuple, mid(), ext)
      case region: Region => OutInRegion(tuple.tuple, mid(), region)
    })

    def rd(tupleTemplate: SituatedTupleTemplate): TupleOpId = TupleOpId(s"${mid()}_rd_$k")(tupleTemplate.situation match {
      case AroundMe(ext) => Read(tupleTemplate.tupleTemplate, mid(), extension = ext)
      case region: Region => Read(tupleTemplate.tupleTemplate, mid(), extension = 20) // TODO
    })

    def in(tupleTemplate: SituatedTupleTemplate): TupleOpId = TupleOpId(s"${mid()}_in_$k")(tupleTemplate.situation match {
      case AroundMe(ext) => In(tupleTemplate.tupleTemplate, mid(), extension = ext)
      case region: Region => In(tupleTemplate.tupleTemplate, mid(), extension = 20) // TODO
    })

    implicit class RichProcessOutput(pout: Map[TupleOpId,OperationResult]) {
      def continue(continuation: OperationResult => TupleOpId): Map[TupleOpId,OperationResult] = {
        sspawn(tupleOperation _, pout.map(v => continuation(v._2).prepend(v._1.uid)).toSet, ProcArg())
      }

      def andNext(continuation: Tuple => Unit): Unit = {
        pout.foreach(v => v._2.result.foreach(r => continuation(r)))
      }

      def evolve(continuation: Tuple => TupleOpId): Map[TupleOpId,OperationResult] = {
        sspawn(tupleOperation _, pout.flatMap(v => v._2.result.map(continuation(_).prepend(v._1.uid))).toSet, ProcArg())
      }
    }

    implicit class RichTupleOpId(toid: TupleOpId) {
      def prepend(s: String): TupleOpId = toid.copy(uid = s"${s}_${toid.uid}")(toid.op)
    }

    implicit class RichTuple(tuple: Tuple) {
      def @@(situation: SpatialSituation): SituatedTuple = SituatedTuple(tuple, situation)
    }

    implicit class RichTupleTemplate(template: TupleTemplate) {
      def @@@(situation: SpatialSituation): SituatedTupleTemplate = SituatedTupleTemplate(template, situation)
    }
  }


  def consensusOn[T](data: Option[T], leader: Boolean, potential: Double): Boolean = {
    val collect = C[Double,Set[T]](potential, _++_, data.toSet, Set.empty)
    val choice = rep[Option[T]](None){ v => v.orElse(collect.headOption) }
    broadcastUnbounded(leader && !choice.isEmpty, choice) == data && !data.isEmpty
  }

  def consensus[T](data: Option[T], leader: Boolean, potential: Double): Option[T] = {
    val collect: Set[T] = C[Double,Set[T]](potential, _++_, data.toSet, Set.empty)
    // if(leader && !collect.isEmpty){ println(s"${mid} > GOT REQUESTS FROM $collect") }
    val choice = rep[Option[T]](None){ c => // if(leader && !collect.isEmpty && c.isDefined){ println(s"${mid} > old was $c") };
      c.filter(collect.contains(_)).orElse(collect.headOption)
    }
    broadcastUnbounded(leader && !choice.isEmpty, if(leader) choice else None)
  }

  // Same as consensus() but once the leader selects one value, it keeps it
  def consensusAndKeep[T](data: Option[T], leader: Boolean, potential: Double): Option[T] = {
    val collect: Set[T] = C[Double,Set[T]](potential, _++_, data.toSet, Set.empty)
    // if(leader && !collect.isEmpty){ println(s"${mid} > GOT REQUESTS FROM $collect") }
    val choice = rep[Option[T]](None){ c => // if(leader && !collect.isEmpty && c.isDefined){ println(s"${mid} > old was $c") };
      c.filter(collect.contains(_)).orElse(collect.headOption)
    }
    val leaderChoice = branch(leader){ keep{ choice }} { None }
    // if(leader && leaderChoice.isDefined){ println(s"${mid} > ${leaderChoice} is the leader choice ")}
    broadcastUnbounded(leader && !leaderChoice.isEmpty, leaderChoice)
  }

  def tupleOperation(toid: TupleOpId)(arg: ProcArg): (OperationResult, Status) = {
    val firstTime = rep(0)(k => if(k==0) 1 else 2) == 1
    branch(toid.op.initiator==mid() && firstTime && node.has(toid.uid)){ // prevent reentrance
      node.put(toid.uid, 2)
      (OperationResult(OperationStatus.completed,None), External)
    } {
      node.put(toid.uid, 1) // marker that a process instance has been executed

      val res = toid.op match {
        case outop@OutMe(s, initiator, extension) => OutMeLogic(toid, outop, arg)
        case outop@OutInRegion(s, initiator, region) => OutInRegionLogic(toid, outop, arg)
        case outop@OutHere(s, initiator, position, extension) => OutInRegionLogic(toid, OutInRegion(s, initiator, CircularRegion(position, extension)), arg)
        case readop@Read(ttemplate, initiator, extension) => ReadLogic(toid, readop, arg)
        case inop@In(ttemplate, initiator, extension) => InLogic(toid, inop, arg)
        case _ => ??? // (OperationResult("invalid"), Terminated)
      }

      val status = res._2
      node.put(toid.uid + "_op", toid.op + s"{$k}")
      node.put(toid.uid + "_status", (if (status == Output) 2 else if (status == Bubble) 1 else if (status == Terminated) 3 else 0) + s" {$k}")
      // println(s"[$mid] $toid -> $res")
      res
    }
  }

  /*
  override def sspawn[A, B, C](process: A => B => (C, Status), params: Set[A], args: B): Map[A,C] = {
    spawn[A,B,Option[C]]((p: A) => (a: B) => {
      val (finished, result, status) = rep((false, none[C], false)) { case (finished, _, _) => {
        val (result, status) = process(p)(a)
        val newFinished = status == Terminated | includingSelf.anyHood(nbr{finished})
        val terminated = includingSelf.everyHood(nbr{newFinished})
        val (newResult, newStatus) = (result, status) match {
          case _ if terminated     => (None, false)
          case (_,     External)   => (None, false)
          case (_,     Terminated) => (None, true)
          case (value, Output)     => (Some(value), true)
          case (_,     Bubble)     => (None, true)
        }
        (newFinished, newResult, newStatus)
      } }
      val exitTuple = s"""exit("${p}")"""
      if(!status){ addTupleIfNotAlreadyThere(exitTuple) } else { removeTuple(exitTuple) }
      (result, status)
    }, params, args).collect { case (k, Some(p)) => k -> p }
  }
   */

  def OutMeLogic(toid: TupleOpId, outOp: OutMe, arg: ProcArg): (OperationResult, Status) = {
    val OutMe(s, initiator, extension) = outOp
    val g = classicGradient(initiator==mid())
    val inRegion = g<=extension
    var nrounds = rep(0)(_+1)

    val terminate = branch(inRegion){ handleRemovalByIN(toid, initiator, s, g) }{ false }
    branch(inRegion){ once{ addTupleIfNotAlreadyThere(s) } }{ None }
    branch(terminate){ once {
      // println(s"${mid} > OUT > remove tuple $s");
      removeTuple(s)
    } }{ None }
    // if(terminate){ println(s"${toid.uid} > ${mid} > OUT $s > terminate [$toid] {$k}") }
    if(mid()==initiator && nrounds==1) {
      node.extendSetWith("outs", toid.uid)
    }
    if(mid()==initiator && terminate){
      node.extendSetWith("outs_closed", toid.uid)
    }
    (OperationResult(OperationStatus.completed, Some(s)), if(terminate) Terminated else if(inRegion) Output else External)
  }

  def handleRemovalByIN(toid: TupleOpId, initiator: ID, s: Tuple, g: Double): Boolean = {
    object OutPhase extends Enumeration {
      type OutPhase = Value
      val NORMAL, SERVING, CLOSING = Value
    }
    import OutPhase._

    val (phase,inproc) = rep[(OutPhase.Value,Option[String])]((NORMAL,None))(p => {
      var (phase,oldRetriever) = p
      // val retrieverStableFor = stableFor(oldRetriever)
      val leader = initiator==mid()
      var processesWhoDidIN: List[Map[String, String]] = solutionsWithMatches(TupleRemovalRequested.matchTemplate).map(_.map(tp => tp._1 -> tp._2.stripQuotes))
      // println(s"${toid.uid} > ${mid} > OUT > $processesWhoDidIN asked for tuple $s")
      processesWhoDidIN = processesWhoDidIN.filter(retrieverMap => {
        val pid = retrieverMap(TupleRemovalRequested.By).stripQuotes
        val template = retrieverMap(TupleRemovalRequested.Template).stripQuotes
        val forThisTuple = new Prolog().`match`(Term.createTerm(s), Term.createTerm(template))
        val exited = exists(s"""exit(${pid.quoted})""") && !exists(TupleRemovalDone.matchingTemplate(pid.quoted,toid.uid.quoted))
        if (exited){ removeTuple(TupleRemovalRequested.matchingTemplate(pid.quoted, template.quoted)) }
        !exited && forThisTuple
      })
      // println(s"${toid.uid} > ${mid} > OUT > choosing the first of filtered $processesWhoDidIN")
      val retrieverData: Option[Map[String, String]] =  processesWhoDidIN.headOption // keepUnless[Map[String, String]](processesWhoDidIN.headOption, r => !processesWhoDidIN.exists(_.get(TupleRemovalRequested.By) == r))
      val retriever = retrieverData.flatMap(_.get(TupleRemovalRequested.By))
      val template: Option[TupleTemplate] = retrieverData.flatMap(_.get(TupleRemovalRequested.Template))
      val chosenRetriever: Option[String] = consensus(data = retriever, leader = leader, potential = g)
      // chosenRetriever.foreach(r => println(s"${toid.uid} > $mid > OUT > $s > consensus on retriever: $r {$k}"))
      val supposedRetriever = chosenRetriever.getOrElse("").quoted
      if (leader) {
        val alreadyGivenTo: Option[String] = solveWithMatch(s"alreadyGiven(${toid.uid.quoted},By)", "By").map(_.stripQuotes)
        val exited = alreadyGivenTo.map(pid => exists(s"""exit(${pid.quoted})""")
          && !exists(TupleRemovalDone.matchingTemplate(pid.quoted,toid.uid.quoted))).getOrElse(false)
        if(exited){
          // println(s"${toid.uid} > ${mid} > already given to ${alreadyGivenTo} - exited? $exited")
          val markToRemove = s"alreadyGiven(${toid.uid.quoted},${alreadyGivenTo.get.quoted})"
          removeTuple(markToRemove)
          val eventToRemove = TupleRemovalOk(toid.uid, alreadyGivenTo.get, s)
          retractEvent(toid, eventToRemove)
          // println(s"${toid.uid} > $mid > OUT > however $alreadyGivenTo exited!!!! {$k} Removing ${markToRemove} and event ${eventToRemove}")
        }
        val alreadyGiven = alreadyGivenTo.isDefined && !exited
        // if(alreadyGiven){ println(s"${toid.uid} > $mid > OUT > $s was already given to $alreadyGivenTo {$k}") }
        chosenRetriever.foreach(r => if (!alreadyGiven) {
          addTuple(s"alreadyGiven(${toid.uid.quoted},${supposedRetriever})")
          // println(s"${toid.uid} > $mid > OUT > $s > giving tuple $s to retriever $r {$k}")
          emitEvent(toid, TupleRemovalOk(toid.uid, r, s))
        })
      }
      chosenRetriever.foreach(r => emitEvent(toid, TupleRemovalOk(toid.uid, r, s)))
      val inProcessDone: Map[String, String] = solveWithMatches(TupleRemovalDone.matchingTemplate(TupleRemovalDone.By, toid.uid.quoted)).map(tp => tp._1 -> tp._2.stripQuotes)
      // val inProcessDoneBy = inProcessDone.get(TupleRemovalDone.By)
      if (!inProcessDone.isEmpty) {
        // println(s"${toid.uid} > $mid > OUT > $s > closing since got $inProcessDone {$k}")
        phase = CLOSING
      }
      (phase, chosenRetriever)
    })
    node.put(s"${toid.uid}_phase", phase + " -- " + inproc)
    phase == CLOSING
  }

  def OutInRegionLogic(toid: TupleOpId, outOp: OutInRegion, arg: ProcArg): (OperationResult, Status) = {
    val OutInRegion(s, initiator, region) = outOp
    val pos = currentPosition()
    val inRegion = region.withinRegion(Point2D(pos.x, pos.y))
    branch(inRegion){ once{ addTupleIfNotAlreadyThere(s) } }{ None }
    /* branch(terminate){ once { removeTuple(s) } }{ None } */
    (OperationResult(OperationStatus.completed, Some(s)), if(inRegion) Output else External)
  }

  def ReadLogic(toid: TupleOpId, readOp: Read, arg: ProcArg): (OperationResult, Status) = {
    val ProcArg(localTuples, localProcs) = arg
    val Read(ttemplate, initiator, extension) = readOp

    val g = classicGradient(initiator==mid())
    // node.put(s"${toid.uid}_g", g)
    val tupleFound = solveFirst(ttemplate) // localTupleMatching(localTuples, ttemplate)
    val result  = broadcastUnbounded[Option[TupleSolution]](!tupleFound.isEmpty, tupleFound)
    val (gotIt,canClose) = rep((false,false))(f => {
      val shouldBeDone = mid()==initiator && !result.isEmpty
      (shouldBeDone, f._2 || (shouldBeDone && f._1))
    })
    result.foreach(sol => addTupleIfNotAlreadyThere(sol.solution))
    (OperationResult(if(result.isDefined) OperationStatus.completed else OperationStatus.inProgress, result.map(_.solution)), if(mid()==initiator){
      if(canClose){ Terminated } else if(gotIt) { Output } else { Bubble }
    } else if(g<extension) { Bubble } else { External })
  }

  def InLogic(toid: TupleOpId, inOp: In, arg: ProcArg): (OperationResult, Status) = {
    val ProcArg(localTuples, localProcs) = arg
    val In(ttemplate, initiator, extension) = inOp
    val nrounds = rep(0)(_+1)
    if(mid()==initiator && nrounds==1) { node.extendSetWith("ins", toid.uid) }
    // Note: IN is not trivial: Need consensus on the tuple to remove; As there might be multiple concurrent INs, these must be discriminated by a tuple's owner
    // So, it needs 2 feedback loops for 4 flows of events: 1) TupleRemovalRequested(by,tuple) -- 2) TupleRemovalOk(by) -- 3) TupleRemovalDone -- 4) TupleRemovalEnd
    val g = classicGradient(initiator==mid())
    // val extension: Double = if(nrounds>30) Math.min(ext*2,50) else ext
    node.put(s"${toid.uid}_g", g)
    node.put(s"${toid.uid}_ext", extension)


    trait InPhase
    object InPhase {
      case object InPhaseStart extends InPhase
      val Start: InPhase = InPhaseStart
      case object InPhaseReading extends InPhase
      val Reading: InPhase = InPhaseReading
      case object InPhaseDone extends InPhase
      val Done: InPhase = InPhaseDone
    }
    import InPhase._

    val (phase,by,result) = rep[(InPhase,Option[String],Option[Tuple])]((Start,None,None)){ case (p,outProc,theTuple) => {
      var phase = p
      //if (phase == Start) {
      emitEvent(toid, TupleRemovalRequested(toid.uid, ttemplate))
      //} //else { retractEvent(inRequestToOut) }
      val tuplesToRemove: List[Map[String, String]] = solutionsWithMatches(TupleRemovalOk.matchingTemplate(TupleRemovalOk.By,toid.uid.quoted,TupleRemovalOk.Tuple)).map(_.map(tp => tp._1 -> tp._2.stripQuotes))
      val by = outProc.filter(op => tuplesToRemove.exists(_.get(TupleRemovalOk.By).map(_==op).getOrElse(true)))
        .orElse(tuplesToRemove.headOption.flatMap(_.get(TupleRemovalOk.By)))
      val tuple = theTuple.filter(op => tuplesToRemove.exists(_.get(TupleRemovalOk.Tuple).map(_==op).getOrElse(true)))
        .orElse(tuplesToRemove.headOption.flatMap(_.get(TupleRemovalOk.Tuple))) // get tuple only if this process is the one chosen by the OUT
      // tuple.foreach(tuple => println(s"${toid.uid} > ${mid} > IN > I can remove tuple $tuple from $by {$k}"))
      val result = consensus[(String,Tuple)]((by,tuple).insideOut, mid()==initiator, g)
      // result.foreach { r => println(s"${toid.uid} > ${mid} > IN > consensus on removing tuple ${r._1} from ${r._2} {$k}") }
      if (result.isDefined) phase = Reading

      if (mid() == initiator && result.isDefined) addTupleIfNotAlreadyThere(result.get._2)
      val didRead: Option[(String,Tuple)] = broadcastUnbounded(mid() == initiator, if(mid()==initiator) result else None)
      val intermediaryGotAckFromReader = (for(intermediaryOutProc <- by;
                                              (chosenOutProc,_) <- didRead) yield intermediaryOutProc==chosenOutProc).getOrElse(false)
      if (intermediaryGotAckFromReader) {
        // println(s"${toid.uid} > $mid > IN > the initiator $initiator did read the tuple $tuple: emitting TupleRemovalDone {$k}");
        emitEvent(toid, TupleRemovalDone(toid.uid, didRead.get._1))
      }
      val ensureOutTermination = true // && localProcs.forall(lp => outProcess.map(_!=lp).getOrElse(true)))
      val done = broadcastUnbounded(intermediaryGotAckFromReader, intermediaryGotAckFromReader && ensureOutTermination)
      if (done) phase = Done
      (phase, result.map(_._1), result.map(_._2))
    }}
    node.put(s"${toid}_phase", phase + s" {$k}")
    node.put(s"${toid}_done", (phase==Done) + s" {$k}")

    (OperationResult(if(result.isDefined) OperationStatus.completed else OperationStatus.inProgress, result.filter(_ => phase==Done)), branch(mid()==initiator){
      branch(phase==Done) {
        // println(s"${toid.uid} > $mid [$initiator] > IN > Done [$phase] - got $result {$k}")
        val newStatus = delay(Terminated,Output,2) // delay termination one round to allow
        if(newStatus == Terminated){ clearEvents(toid) }
        if(nrounds==1){ print("*"); External } else newStatus
      }{ Bubble } // if(mid==initiator) Output else Bubble
    } { if(g<extension) Bubble else External })
  }

  var eventMaps: Map[TupleOpId,Set[String]] = Map.empty
  def emitEvent(toid: TupleOpId, event: ProcessEvent): Unit = {
    eventMaps += toid -> (eventMaps.getOrElse(toid,Set.empty) + event.toString)
    addTupleIfNotAlreadyThere(event.toString)
  }
  def clearEvents(toid: TupleOpId): Unit = {
    eventMaps.get(toid).foreach(t => t.foreach(removeTuple(_)))
    eventMaps += toid -> Set.empty
  }
  def retractEvent(toid: TupleOpId, event: ProcessEvent): Unit = removeTuple(event.toString)

  def broadcastUnbounded[V](source: Boolean, field: V): V =
    G[V](source, field, v => v, nbrRange _)

  // TODO: add to scafi incarnation

  implicit class RichNodeManager(nodeManager: NodeManager){
    def extendSetWith[T](name: String, value: T) =
      node.put[Set[T]](name, if(!node.has(name)) Set(value) else node.get[Set[T]](name)+value)

    def countSet(name: String) =
      if(!node.has(name)) 0 else node.get[Set[Any]](name).size

    def getOption[T](name: String): Option[T] = if(node.has(name)) Some(node.get[T](name)) else None
  }

  // TODO: add to stdlib
  def once[T](expr: => T): Option[T] = rep((true,none[T])){ case (first,value) => (false, if(first) Some(expr)  else None) }._2
  def none[T]: Option[T] = None
  def keep[T](expr: Option[T]): Option[T] = rep[Option[T]](None){ _.orElse(expr) }
  def keepUnless[T](expr: Option[T], reset: T=>Boolean): Option[T] = rep[Option[T]](None){ old => if(old.map(reset).getOrElse(false)) {
    // print("#")
    None
  } else old.orElse(expr) }
  def keep(expr: Boolean): Boolean = rep(false)(b => if(b) b else expr)
  def unchanged[T](value: T): Boolean = rep(value,true)( v => (value,value==v._1))._2
  def delay[T](value: T, default: T, nrounds: Int = 1): T = rep((default,default,0)){
    case (old,_,k) => (if(k<nrounds) old else value, old, k+1)
  }._2
  def goesUp(c: Boolean): Boolean = rep((false,false)){ case (oldRes,oldC) => (!oldC && c, c) }._1
  def stableFor[T](value: T): Int = rep((value,0)){ case (old,k) => (value, if(old==value) k+1 else 0) }._2

  implicit class MyRichStr(s: String){
    def stripQuotes = s.stripPrefix("'").stripSuffix("'")
    def quoted = s"'$s'"
  }

  // Utility stuff
  implicit class Tuple2Opt[A,B](t: (Option[A],Option[B])){
    def insideOut: Option[(A,B)] = for(a <- t._1; b <- t._2) yield (a,b)
  }

  // TODO: fix remove to perform the check
  def removeMolecule(name: String) = if(node.has(name)) node.remove(name)

}