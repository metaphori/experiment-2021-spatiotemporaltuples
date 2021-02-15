package it.unibo.alchemist.model.implementations.layers

import it.unibo.alchemist.model.interfaces.Layer
import it.unibo.alchemist.model.interfaces.Position

@SerialVersionUID(1L)
final class UniformLayer[T, P <: Position[P]](var level: T)

/**
 * @param level
 * the concentration
 */
  extends Layer[T, P] {
  override def getValue(p: P) = level

  def setValue(v: T) = this.level = v
}