package datacenter

import problem._

trait Solver {
  def solve(problem: Problem): Solution
}

object SequentialSolver extends Solver {
  def solve(problem: Problem): Solution = {
    val freeSlots = problem.initialFreeSlot
    var totalSize = freeSlots.map(_.size).sum
    println(s"${freeSlots.size} slots for a total size of $totalSize")
    val servers = problem.servers
    val bestServers = servers.sortBy(s => -s.ratio).takeWhile { s => totalSize -= s.size; totalSize >= 0 }
    val bestCapacity = bestServers.map(_.capacity).sum
    val bestCapPerPool = bestCapacity / problem.nbPools
    val bestAvailCapPerPool = bestCapPerPool * (problem.nbRows - 1) / problem.nbRows
    println(s"maximum total capacity : $bestCapacity for ${problem.nbPools} pools => $bestCapPerPool per pool, max available with 1 out of ${problem.nbRows} rows down : $bestAvailCapPerPool")

    val big = bestServers.filter(_.size > 2).sortBy(-_.capacity)
    val small = bestServers.filter(_.size == 2).sortBy(-_.capacity)
    val last = servers.filter(_.size == 1).sortBy(-_.capacity)
    val sortedServers = big ::: small ::: last

    def selectPool(server: Server, solution: Solution): Pool = {
      val pools = 0 until problem.nbPools
      val similarServersUsed = solution.actuallyAllocated.toList.sortBy { case (s, a) => math.abs(s.capacity - server.capacity) }
      val poolsWithSimilarBigServer = for {
        pool <- pools
        if solution.capacity(pool) + server.capacity < 400
        servers = solution.serversPerPool(pool)
        if servers.nonEmpty
        maxServer = servers.toMap.keys.maxBy(_.capacity)
        delta = math.abs(maxServer.capacity - server.capacity)
      } yield pool -> delta
      poolsWithSimilarBigServer.sortBy(_._2).headOption match {
        case Some((pool, delta)) if delta < 5 => pool
        case _ =>
          pools.minBy(p => solution.scoreForPool(p) + solution.capacity(p) / 100.0)
      }
    }

    def solveRec(freeSlots: List[FreeSlot], servers: List[Server], solution: Solution): Solution = {
      servers match {
        case Nil => solution
        case server :: t =>
          val pool = selectPool(server, solution)
          val rowsByCapacityForPool = (0 until problem.nbRows).sortBy { row =>
            solution.capacity(pool, row)
          }
          val slots = freeSlots
            .sortBy(-_.size)
            .sortBy(slot => rowsByCapacityForPool.indexOf(slot.coord.row))
          slots.find(_.size >= server.size) match {
            case None =>
              println(s"no slot free for $server")
              if (server.size == 1) solution
              else solveRec(freeSlots, t, solution.allocate(server, None))
            case Some(slot) =>
              val remainingSlot = slot.use(server.size)
              val newSolution = solution.allocate(server, Some(Allocation(slot.coord, pool)))
              val r = slot.coord.row
              val c = slot.coord.slot
              println(s"pool $pool : setting $server at ${r}x${c} (score for pool: ${solution.scoreForPool(pool)} => ${newSolution.scoreForPool(pool)})")
              solveRec(
                remainingSlot :: (freeSlots diff List(slot)),
                t,
                newSolution)
          }
      }
    }
    solveRec(freeSlots, sortedServers, Solution(problem, Map.empty))
  }
}