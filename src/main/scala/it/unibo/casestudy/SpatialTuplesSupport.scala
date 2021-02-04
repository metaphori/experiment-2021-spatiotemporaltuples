package it.unibo.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ID
import it.unibo.scafi.space.Point2D

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

  case class TupleOpId(uid: String)(val op: TupleOp, val issuedAtTime: Double) {
    override def toString: Tuple = uid
  }

  trait OperationStatus

  object OperationStatus {
    val inProgress: OperationStatus = new OperationStatus {}
    val completed: OperationStatus = new OperationStatus {}
  }

  case class OperationResult(operationStatus: OperationStatus, result: Option[Tuple])

  case class ProcArg(localTuples: Set[Tuple] = Set.empty, procs: Set[TupleOpId] = Set.empty)

  trait ProcessEvent

  case class TupleRemovalRequested(by: String, tuple: TupleTemplate) extends ProcessEvent {
    override val toString: TupleTemplate = s"""inRequested(by("${by}"),template("$tuple"))"""
  }

  object TupleRemovalRequested {
    val By = "Uid"
    val Template = "Template"
    val variables: List[String] = List(By, Template)

    def matchingTemplate(by: String, template: String): TupleTemplate = s"inRequested(by($by),template($template))"

    val matchTemplate = matchingTemplate(By, Template)
  }

  case class TupleRemovalOk(by: String, to: String, tuple: Tuple) extends ProcessEvent {
    override val toString: TupleTemplate = s"""inOk(by("${by}"),to("$to"),tuple("$tuple"))"""
  }

  object TupleRemovalOk {
    val By = "Uid"
    val To = "Template"
    val Tuple = "Tuple"
    val variables: List[String] = List(By, To, Tuple)

    def matchingTemplate(by: String, to: String, tuple: Tuple): TupleTemplate = s"inOk(by($by),to($to),tuple($tuple))"

    val matchTemplate = matchingTemplate(By, To, Tuple)
  }

  case class TupleRemovalDone(by: String, to: String) extends ProcessEvent {
    override val toString: TupleTemplate = s"""inDone(by("${by}"),to("$to"))"""
  }

  object TupleRemovalDone {
    val By = "Uid"
    val To = "Template"
    val variables: List[String] = List(By, To)

    def matchingTemplate(by: String, to: String): TupleTemplate = s"inDone(by($by),to($to))"

    val matchTemplate = matchingTemplate(By, To)
  }

}