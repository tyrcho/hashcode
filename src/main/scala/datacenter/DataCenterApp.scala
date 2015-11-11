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

  val scores=for {
    maxScore <- (410 to 450).par 
    solver= SequentialSolver(maxScore)
    solution=solver.solve(problem)
    score=solution.score
    msg=s"parameter maxScore=$maxScore => score = $score"
  } yield {
    println(msg)
    msg
  }
  
  scores.foreach(println)
  
  
//  val sol = SequentialSolver.solve(problem)
//  sol.validate()
//
//  for {
//    i <- 0 until problem.nbPools
//  } println(sol.debugPool(i))
//
//  sol.debug.foreach(println)
//
//  import java.io._
//  val writer = new FileWriter("out.txt")
//  sol.format.foreach(writer.write)
//  writer.flush()
//  writer.close()
//
//  println("total capacity allocated : " + sol.serversAllocated.filterNot(_._2.isEmpty).keys.toList.map(_.capacity).sum)
//
//  println(sol.score)

}