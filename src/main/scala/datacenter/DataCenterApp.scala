package datacenter

import problem._

object DataCenterApp extends App {

  val input = io.Source.fromFile("input/data.in").getLines.toList
  val first = input.head
  val Array(nbRows, nbSlots, nbUnavailable, nbPools, nbServers) = first.split(" ").map(_.toInt)

  val servers = for {
    (line, i) <- input.tail.drop(nbUnavailable).take(nbServers).zipWithIndex
    Array(s, c) = line.split(" ")
  } yield Server(s.toInt, c.toInt, i)

  val unavailable = for {
    line <- input.tail.take(nbUnavailable)
    Array(r, s) = line.split(" ")
  } yield UnavailableSlot(Coord(r.toInt, s.toInt))

  val problem = Problem(nbRows, nbSlots, servers, unavailable, nbPools)

  val sol = SequentialSolver.solve(problem)
  sol.format.foreach(println)
  sol.validate()
  println(sol.score)

  for {
    i <- 0 until problem.nbPools
  } println(sol.debugPool(i))
  
  sol.debug.foreach(println)
}