import DataCenterProblem.problem

object DatacenterApp extends App {

  val sol = SequentialSolver.solve(problem)
  sol.format.foreach(println)
  problem.validate(sol)
  println(problem.score(sol))
val pools=  sol.poolsPerCapacity(problem.nbPools).toMap
  for {
    i <- 0 until problem.nbPools
    capaPool=pools(i)
    servers = sol.serversPerPool(i)
    maxCapa = servers.maxBy(_.capacity)
  } println(s"pool $i with capa $capaPool : max server : $maxCapa")
}