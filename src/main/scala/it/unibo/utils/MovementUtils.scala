package it.unibo.utils

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.space.{Point2D, Point3D}

trait MovementUtils {
  self: AggregateProgram with StandardSensors with ScafiAlchemistSupport =>

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
}
