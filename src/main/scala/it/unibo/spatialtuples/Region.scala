package it.unibo.spatialtuples

import it.unibo.scafi.space.{Point2D, Point3D}

trait SpatialSituation
object SpatialSituation {
  implicit def situateTemporally(s: SpatialSituation): SpatiotemporalSituation =
    SpatiotemporalSituation(s, Forever)
}

case class AroundMe(extension: Double) extends SpatialSituation
object Me extends AroundMe(0)
object Everywhere extends AroundMe(Double.PositiveInfinity)

trait TemporalSituation
case object Forever extends TemporalSituation

case class SpatiotemporalSituation(spatialSituation: SpatialSituation,
                                   temporalSituation: TemporalSituation)

trait Region extends SpatialSituation {
  def withinRegion(p: Point3D): Boolean
  def center: Point2D
}

case class RectangularRegion(start: Point2D, end: Point2D) extends Region {
  override def withinRegion(p: Point3D): Boolean = {
    p.x >= start.x && p.x <= end.x && p.y >= start.y && p.y <= end.y && p.z >= start.z && p.z <= end.z
  }

  def center: Point2D = Point2D((start.x + end.x)/2.0, (start.y + end.y)/2.0)
}

case class CircularRegion(center: Point2D, radius: Double) extends Region {
  override def withinRegion(p: Point3D): Boolean = {
    Math.sqrt(Math.pow(p.x - center.x, 2) + Math.pow(p.y - center.y, 2)) <= radius
  }
}