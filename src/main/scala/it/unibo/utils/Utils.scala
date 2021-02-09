package it.unibo.utils

import it.unibo.alchemist.model.implementations.nodes.NodeManager
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

trait Utils extends BlockG with BlockC { self: AggregateProgram with ScafiAlchemistSupport with StandardSensors =>

  def T = alchemistTimestamp.toDouble.toLong

  def branchOn[V,O](v: V)(f: V => O): O =
    align(v){ f(_) }

  def broadcastOn[V](g: Double, v: V): V =
    G_along[V](g, nbrRange _, v, v => v)

  def gossip[V](source: Boolean, field: V): V =
    G[V](source, field, v => v, nbrRange _)

  def consensusOn[T](data: Option[T], leader: Boolean, potential: Double): Boolean = {
    val collect = C[Double,Set[T]](potential, _++_, data.toSet, Set.empty)
    val choice = rep[Option[T]](None){ v => v.orElse(collect.headOption) }
    gossip(leader && !choice.isEmpty, choice) == data && !data.isEmpty
  }

  def consensus[T](data: Option[T], leader: Boolean, potential: Double): Option[T] = {
    val collect: Set[T] = C[Double,Set[T]](potential, _++_, data.toSet, Set.empty)
    // if(leader && !collect.isEmpty){ println(s"${mid} > GOT REQUESTS FROM $collect") }
    val choice = rep[Option[T]](None){ c => // if(leader && !collect.isEmpty && c.isDefined){ println(s"${mid} > old was $c") };
      c.filter(collect.contains(_)).orElse(collect.headOption)
    }
    gossip(leader && !choice.isEmpty, if(leader) choice else None)
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
    gossip(leader && !leaderChoice.isEmpty, leaderChoice)
  }

  // TODO: add to stdlib
  def once[T](expr: => T): Option[T] = rep((true,none[T])){ case (first,value) => (false, if(first) Some(expr)  else None) }._2
  def none[T]: Option[T] = None
  def keep[T](expr: Option[T]): Option[T] = rep[Option[T]](None){ _.orElse(expr) }
  def keepUnless[T](expr: Option[T], reset: T=>Boolean): Option[T] = rep[Option[T]](None){ old => if(old.map(reset).getOrElse(false)) {
    // print("#")
    None
  } else old.orElse(expr) }
  def keepUntil[T](value: T, until: T => Boolean): T = rep(value)(old => if(until(old)) value else old)
  def keep(expr: Boolean): Boolean = rep(false)(b => if(b) b else expr)
  def unchanged[T](value: T): Boolean = rep(value,true)( v => (value,value==v._1))._2
  def delay[T](value: T, default: T, nrounds: Int = 1): T = rep((default,default,0)){
    case (old,_,k) => (if(k<nrounds) old else value, old, k+1)
  }._2
  def goesUp(c: Boolean): Boolean = rep((false,false)){ case (oldRes,oldC) => (!oldC && c, c) }._1
  def stableFor[T](value: T): Int = rep((value,0)){ case (old,k) => (value, if(old==value) k+1 else 0) }._2
  def trueOnFirstExecutionOnly() = rep(0)(k => if(k==0) 1 else 2) == 1

  implicit class MyRichStr(s: String){
    def stripQuotes = s.stripPrefix("'").stripSuffix("'")
    def quoted = s"'$s'"
  }

  // Utility stuff
  implicit class Tuple2Opt[A,B](t: (Option[A],Option[B])){
    def insideOut: Option[(A,B)] = for(a <- t._1; b <- t._2) yield (a,b)
  }

  def inc(name: String) = node.put[Int](name, node.getOrElse(name, 0)+1)

  def extendSet[V](name: String, v: V) = node.put[Set[V]](name, node.getOrElse[Set[V]](name, Set.empty)+v)


  implicit class RichNodeManager(nodeManager: NodeManager){
    def extendSetWith[T](name: String, value: T) =
      node.put[Set[T]](name, if(!node.has(name)) Set(value) else node.get[Set[T]](name)+value)

    def countSet(name: String) =
      if(!node.has(name)) 0 else node.get[Set[Any]](name).size

    def getOption[T](name: String): Option[T] = if(node.has(name)) Some(node.get[T](name)) else None
  }
}
