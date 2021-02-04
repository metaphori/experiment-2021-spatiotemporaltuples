package it.unibo.casestudy

import it.unibo.alchemist.model.interfaces.Position
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.space.{Point2D, Point3D}
import org.scalactic.TripleEquals._
import org.scalactic.Tolerance._

class TestAggregateProcesses extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with CustomSpawn with Gradients {
  import SpawnInterface._

  /**
   * This program realises simple spawning of gradient processes.
   * The "difficult" part lies in controlling the processes' lifecycle.
   */
  override def main(): Any = {

    val t = alchemistTimestamp.toDouble.toLong
    val procs = node.get[Map[Int,(Int,Int)]]("procs")
    val pids: Set[Pid] = if(procs.contains(t.toInt)){
      if(procs(t.toInt)._1 == mid()) Set(Pid()(terminateAt=procs(t.toInt)._2)) else Set.empty
    } else Set.empty

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

  /**
   * // Crop a coordinate to fit into a rectangle.
   * def cropRectangle(x, low, hi) =
   * [min(max(x.get(0), low.get(0)), hi.get(0)),
   * min(max(x.get(1), low.get(1)), hi.get(1))]
   *
   * // Random vector of norm up to r (biased towards the border).
   * def randVector(r) {
   * let theta = 2*PI*self.nextRandomDouble();
   * pow(self.nextRandomDouble(), 0.2) * r * [cos(theta),sin(theta)]
   * }
   *
   * // Random vector within the rectangle bounded by points "lo" and "hi".
   * def randRect(lo, hi) =
   * [lo.get(0) + (hi.get(0)-lo.get(0))*self.nextRandomDouble(),
   * lo.get(1) + (hi.get(1)-lo.get(1))*self.nextRandomDouble()]
   *
   * // Random vector within the rectangle bounded by points "lo" and "hi".
   * def randCircle(center, radius) = center + randVector(radius)
   *
   *
   * // Returns a goal by applying function "goal", and regenerates it whenever
   * // the distance from the current goal drops below "mindist".
   * def ifClose(goal, dist) =
   * rep (x <- goal()) {
   * // if (self.distanceTo(x) <= dist)
   * if (computeDistance(self, x) <= dist)
   * { goal() } else { x }
   * }
   *
   * // Walk to random targets within a rectangle of given size (and within a range if given), changing targets within reach.
   * public def rectangleWalkRange(lo, hi, dspace, reach) =
   * env.put('target', ifClose({cropRectangle(self.getCoordinates()+randVector(dspace), lo, hi)}, reach))
   */
  def rectangleWalk() = {
    val goal = randomPoint()
    node.put("target", ifClose(cropRectangle(goal, Point2D(0,0), Point2D(1000,1000))))
  }
  def cropRectangle(goal: Point2D, rect1: Point2D, rect2: Point2D): Point2D = {
    Point2D(if(goal.x < rect1.x) rect1.x else if(goal.x > rect2.x) rect2.x else goal.x,
      if(goal.y < rect1.y) rect1.y else if(goal.y > rect2.x) rect2.y else goal.y)
  }
  def randomPoint(): Point2D = {
    val p = currentPosition()
    Point2D(p.x + 50 * (nextRandom()-0.5), p.y + 50 * (nextRandom()-0.5))
  }
  implicit class RichPoint3D(p: Point3D) {
    def toAlchemistPosition: P = alchemistEnvironment.makePosition(p.productIterator.map(_.asInstanceOf[Number]).toSeq:_*).asInstanceOf[P]
  }
  implicit def toPoint2D(p: Point3D): Point2D = Point2D(p.x, p.y)
  def ifClose(goal: Point3D, dist: Double = 1): Point2D = {
    rep(goal)(g => if(currentPosition().distance(g) <= dist) goal else g )
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