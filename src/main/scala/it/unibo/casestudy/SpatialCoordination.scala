package it.unibo.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.space.{Point2D, Point3D}
import it.unibo.spatialtuples.{CircularRegion, SpatialTuplesSupport}
import it.unibo.spatialtuples.SpatialTuplesSupport._
import it.unibo.utils.MovementUtils
import org.apache.commons.math3.distribution.ExponentialDistribution
import org.scalactic.TripleEquals._
import org.scalactic.Tolerance._

class SpatialCoordination extends AggregateProgram with StandardSensors with CustomSpawn with Gradients with MovementUtils with SpatialTuplesSupport
  /*with LindaDSL*/ with FieldUtils {
  import SpatialCoordination._
  import SpawnInterface._

  lazy val expDistibution = new ExponentialDistribution(alchemistRandomGen, 1.0)

  val teamSize = 5
  lazy val EXPLORER = sense[Int](Molecules.ROLE_EXPLORER)==1
  lazy val FIRST_EXPLORER = EXPLORER && mid() < teamSize
  lazy val SECOND_EXPLORER = EXPLORER && mid() >= teamSize
  lazy val VICTIM = sense[Int](Molecules.ROLE_VICTIM)==1
  lazy val OBSTACLE = sense[Int](Molecules.ROLE_OBSTACLE)==1
  lazy val NODE = !EXPLORER && !VICTIM && !OBSTACLE
  def UNAVAILABLE: Boolean = node.getOrElse(Molecules.UNAVAILABLE, 0)==1

  lazy val lazy_init: Unit = {
    node.put(Molecules.UNAVAILABLE, 0)
  }


  def initialise() = {
    initialiseExports()
    initialiseEffects()
    lazy_init
    node.put(Molecules.MOVE_TOWARDS, currentPosition())
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
    node.put(Effects.FIRST_EXPLORER, FIRST_EXPLORER)
    node.put(Effects.SECOND_EXPLORER, SECOND_EXPLORER)
  }

  def closeEnough(curr: (TupleOpId, TupleOpResult)): Boolean = curr._1.op match {
    case OutHere(tp,by,pos,ext) => pos.distance(currentPosition()) < Constants.CLOSENESS_TO_BREADCRUMB
  }

  override def main(): Any = {
    initialise()

    val meanYLevel = -200 + (mid() % teamSize) * (500 / teamSize) // Y level in the map for team-wide exploration of the map
    val horizon = Point2D(1000,meanYLevel)

    branch(!UNAVAILABLE) {
      val victimFound = minHood(nbrRange() * nbr(if(VICTIM) 1 else Double.PositiveInfinity)) < Constants.VICTIM_RANGE && EXPLORER
      node.put(Exports.VICTIM_FOUND, victimFound)

      val target: Point2D = excludingSelf.minHoodSelector(toMinimize = nbrRange() * nbr(if(VICTIM) 1 else Double.PositiveInfinity))(data =
        (nbrRange() * nbr(if(VICTIM) 1 else Double.PositiveInfinity), currentPosition() + nbrVector())) // nearest victim
        .filter(tp => !tp._1.isInfinite && tp._1 < 75 /* if victim not enough close, continue exploration */)
        .map(_._2).getOrElse(horizon) // or an horizon point
      node.put("target_point", target)

      // MOVEMENT LOGIC
      branch(FIRST_EXPLORER && !victimFound && currentPosition().x < Constants.MAX_X) {
        moveTo(exploreTowards(target))
      } {
        moveTo(currentPosition())
      }

      // OBSTACLE HITTING LOGIC
      if (EXPLORER & excludingSelf.anyHood(nbr { OBSTACLE } && nbrRange() < Constants.CRITICAL_VICINITY_OBSTACLE)) {
        node.put(Molecules.UNAVAILABLE, 1)
      }

      val newBreadcrumb: Set[TupleOpId] = if(FIRST_EXPLORER && T%5==0 && !UNAVAILABLE) {
        Set(TupleOpId(s"breadcrumb_${mid()}_${T}")(OutHere(s"wasHere(${mid()},$T,${currentPosition()})", mid(), currentPosition(), Constants.BREADCRUMB_EXTENSION), T.toDouble))
      } else { Set.empty }

      // Breadcrumb logs: these are tuple operation IDs propagated to spread knowledge about where tuple operations are located
      var breadcrumbLogs = sspawn[ID,Unit,Set[TupleOpId]](k => a => {
        val g = distanceTo(mid() == k)
        val set: Set[TupleOpId] = gossipSet[TupleOpId](if(mid() == k) newBreadcrumb else Set.empty).filter(_.op match {
          case OutHere(tp,by,pos,ext) => pos.distance(currentPosition()) < Constants.EXT_PID_DIFFUSION
          case _ => false
        })
        // node.put(s"proc_${k}_g", g)
        // node.put(s"proc_${k}_s", set)
        (set, if(g < Constants.EXT_PID_DIFFUSION || !set.isEmpty) Output else External)
      }, if(FIRST_EXPLORER) Set(mid()) else Set.empty, ())

      node.put("breadcrumbs_logs", breadcrumbLogs)

      if(node.has(Molecules.BREADCRUMB)) node.remove(Molecules.BREADCRUMB)
      breadcrumbLogs.flatMap(_._2).map(_.op match {
        case OutHere(_, by, pos, _) => (by,pos)
      }).minByOption(tp => tp._2.distance(currentPosition())).foreach(tp => node.put(Molecules.BREADCRUMB, tp._1 % teamSize))

      // One process participates to all processes for close breadcrumbs
      val breadcrumbsToSpawn = breadcrumbLogs.flatMap(_._2).filter(toid => toid.op match {
        case OutHere(_, _, pos, _) => {
          //if(mid()==9) { println(s"[${mid()}][t=$T] Considering breadcrumb ${toid}: ${pos.distance(currentPosition())} < ${Constants.EXT_PID_DIFFUSION} ?  ${pos.distance(currentPosition()) < Constants.EXT_PID_DIFFUSION}") }
          pos.distance(currentPosition()) < Constants.EXT_PID_DIFFUSION
        }
      }).toSet

      // TUPLE OPERATIONS VM: OUT of breadcrumbs by first explorers, READ of breadcrumbs by second explorers
      val breadcrumbs = rep(Map[TupleOpId,TupleOpResult]())(ops => {
        val newBreadcrumbs = sspawn[TupleOpId, ProcArg, TupleOpResult](tupleOperation _, breadcrumbsToSpawn, ops)
        newBreadcrumbs
      })
      node.put("breadcrumbs", breadcrumbs)

      branch[Any](SECOND_EXPLORER && T > Constants.T_SECOND_EXPLORERS_DEPARTURE) {
        type B = Option[(TupleOpId,TupleOpResult)]
        val (reached,currentBreadcrumb) = rep[(B,B)]((Option.empty,Option.empty)){ case (reached,curr) => {
          mux(curr.isDefined && !closeEnough(curr.get)) { (reached,curr) } {
            val breadcrumbsForMe = breadcrumbs.keySet.map(toid => (toid, toid.op match {
              case OutHere(_, by, pos, _) => (by, pos)
            }))
              .filter(tp => (tp._2._1 % teamSize) == (mid() % teamSize)) // select breadcrumbs from corresponding team member
            node.put("breadcrumbsForMe", breadcrumbsForMe)
            val selectedBreadcrumbs = breadcrumbsForMe
              .filter(tp => tp._1.issuedAtTime > reached.map(_._1.issuedAtTime).getOrElse(0.0)) // select later breadcrumbs
              .minByOption(tp => tp._2._2.distance(currentPosition()))
            (if(curr.isDefined) curr else reached, selectedBreadcrumbs.map(tp => (tp._1, breadcrumbs(tp._1))))
          }
        } }
        node.put("breadcrumbsReached and Selected", (reached,currentBreadcrumb))
        var currentTarget = currentBreadcrumb.map(_._1.op match { case OutHere(_, _, position, _) => position }).filter(_ != reached)
        if(T < Constants.T_SECOND_EXPLORERS_DEPARTURE+20) { currentTarget = Some(horizon) }
        node.put("target_breadcrumb_loc", currentTarget)
        //moveFollowingBreadcrumbs(breadcrumbs)
        val skipAndFollow = branch(currentTarget.isEmpty) { skipAndFollowLogic(target) } { currentPosition() }
        moveTo(exploreTowards(currentTarget.getOrElse(skipAndFollow), maxRandomStep = 0))
      } {

      }

    }{ /* unavailable devices do not do anything */ }

    def skipAndFollowLogic(eventualTarget: Point2D): Point2D = {
      println(s"[${mid()}]")
      val up = false // nextRandom() > 0.5
      val p = currentPosition()
      rep[(Point2D,Point2D,Boolean)]((p,p,up)){ case (startingPos, _, initiallyUp) => {
        val targetY = if(initiallyUp) startingPos.y + Constants.AVOID_BY_Y else startingPos.y - Constants.AVOID_BY_Y
        val targetX = startingPos.x + Constants.AVOID_BY_X
        val nextTarget: Point2D = if(p.x === startingPos.x +- 0.01 && !(p.y === targetY +- 0.1) ) { Point2D(p.x, targetY) }
          else if(p.x === targetX +- 0.1 && !(p.y === startingPos.y +- 0.1))  { Point2D(p.x, startingPos.y) }
          else if(p.y === targetY +- 0.1) { Point2D(targetX, p.y) }
          else  { eventualTarget }
        (startingPos, nextTarget, initiallyUp)
      }}._2
    }

    def gossipSet[T](newSet: Set[T] = Set.empty): Set[T] = rep(newSet)(s => includingSelf.unionHoodSet(nbr{ s }) ++ newSet)

    // Log stuff
    //node.put("local-tuple-space", tupleSpace.getTheory.getText)
    //node.put("tuple_ops", ops)
    node.put(Exports.NUM_OUTS_CLOSED, node.getOrElse[Set[TupleOpId]](Molecules.OUTS_CLOSED, Set.empty).size)
    node.put(Exports.NUM_INS_CLOSED, node.getOrElse[Set[TupleOpId]](Molecules.INS_CLOSED, Set.empty).size)
    node.put(Exports.NUM_OUTS_TIMEOUT, node.getOrElse[Set[TupleOpId]](Molecules.OUTS_TIMEOUT, Set.empty).size)
    node.put(Exports.NUM_INS_TIMEOUT, node.getOrElse[Set[TupleOpId]](Molecules.INS_TIMEOUT, Set.empty).size)
  }

  import SpawnInterface._
  case class RegionalProcess(initiator: ID, p1: Point2D, p2: Point2D)
  def processInRegion[V](r: RegionalProcess)(args: Unit)(f: => V): (V, Status) = {
    val status = if(r.initiator == mid() || currentPosition().contained(r.p1,r.p2)) Output else External
    ???
  }

  def exploreTowards(horizon: Point2D, maxStep: Double = 20, maxRandomStep: Double = 20): Point2D = {
    val curPos = currentPosition()
    val rp = randomPoint(curPos, maxRandomStep)
    val deltaX = horizon.x-curPos.x
    val deltaY = horizon.y-curPos.y
    val offsetX = if(deltaX > 0) Math.min(deltaX, maxStep/2) else if(deltaX < 0) Math.max(deltaX, -maxStep/2) else 0
    val offsetY = if(deltaY > 0) Math.min(deltaY, maxStep/2) else if(deltaY < 0) Math.max(deltaY, -maxStep/2) else 0
    Point2D(rp.x + offsetX, rp.y + offsetY) // -200 + (mid() % teamSize) * (500 / teamSize) +
  }

  def moveTo(nextTarget: Point2D): Unit =
    node.put(Molecules.MOVE_TOWARDS, nextTarget)


  implicit class RichPoint2D(p: Point2D) {
    def /(d: Double): Point2D = Point2D(p.x/d, p.y/2)
  }

  implicit class AnotherRichPoint3D(p: Point3D) {
    def contained(p1: Point2D, p2: Point2D): Boolean =
      p.x >= p1.x && p.x <= p2.x && p.y >= p1.y && p.y <= p2.y

    def -(p2: Point3D): Point3D = new Point3D(p.x - p2.x, p.y - p2.y, p.z - p2.z)
  }
}

object SpatialCoordination {
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
    // TODO: add the following to the YAML descriptor
    val VICTIM_FOUND: String = "victim_found"
  }
  object Molecules {
    val BREADCRUMB: String = "breadcrumb"
    val UNAVAILABLE: String = "unavailable"
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
    val ROLE_EXPLORER = "explorer"
    val ROLE_OBSTACLE = "obstacle"
    val ROLE_VICTIM = "victim"
    val MOVE_TOWARDS = "target"
  }
  object Effects {
    val FIRST_EXPLORER: String = "first_explorer"
    val SECOND_EXPLORER: String = "second_explorer"
    val OUT_PHASE = "out_phase"
    val IN_PHASE = "in_phase"
    val IN_PHASE2 = "in_phase2"
    val DOING_OUT = "doing_out"
    val DOING_IN = "doing_in"
    val DOING_READ = "doing_read"
    val INITIATOR = "initiator"
  }
  object Constants {
    val AVOID_BY_Y = 60
    val AVOID_BY_X = 100
    val BREADCRUMB_EXTENSION: Double = 100
    val CLOSENESS_TO_BREADCRUMB = 30.0
    val VICTIM_RANGE = 20.0
    val CRITICAL_VICINITY_OBSTACLE = 20.0
    val EXT_PID_DIFFUSION = 150 // the extension of space that PIDs must be propagated to support spatial situation
    val MAX_X = 900
    val T_SECOND_EXPLORERS_DEPARTURE = 50
  }
}