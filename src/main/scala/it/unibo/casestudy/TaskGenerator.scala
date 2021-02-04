package it.unibo.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.casestudy.TupleSupport.addTupleIfNotAlreadyThere
import org.scalactic.Tolerance._
import org.scalactic.TripleEquals._

class TaskGenerator extends AggregateProgram with StandardSensors with ScafiAlchemistSupport {

  override def main(): Any = {
    val k = rep(0)(_+1)
    val ext = alchemistRandomGen.nextInt(60)
    if(mid() % 30 == 0 & alchemistTimestamp === mid().toDouble +- 1.0) {
      addTupleIfNotAlreadyThere(s"taskToGenerate(task(x${mid()}y$k),$ext)")
    }
  }
}