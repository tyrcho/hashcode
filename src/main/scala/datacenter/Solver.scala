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
    val bestServers=problem.servers.sortBy(s => -s.ratio).takeWhile { s => totalSize -= s.size; totalSize >= 0 }
    val bestCapacity=bestServers.map(_.capacity).sum
    val bestCapPerPool=bestCapacity/problem.nbPools
    val bestAvailCapPerPool=bestCapPerPool*(problem.nbRows-1)/problem.nbRows
    println(s"maximum total capacity : $bestCapacity for ${problem.nbPools} pools => $bestCapPerPool per pool, max available with 1 out of ${problem.nbRows} rows down : $bestAvailCapPerPool")
    
    val sortedServers = problem.servers.sortBy(s => s.ratio + s.size).reverse

    def solveRec(freeSlots: List[FreeSlot], servers: List[Server], solution: Solution): Solution = {
      servers match {
        case Nil => solution
        case server :: t =>

          val pools = 0 until problem.nbPools
          val pool = pools.minBy(p => solution.scoreForPool(p) + solution.capacity(p) / 100.0)
          val rowsByCapacityForPool = (0 until problem.nbRows).sortBy { row =>
            solution.capacity(pool, row) + solution.rowCapacity(row)
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
              println(s"setting $server at $slot for pool $pool (score : ${solution.scoreForPool(pool)})")
              solveRec(
                remainingSlot :: (freeSlots diff List(slot)),
                t,
                solution.allocate(server, Some(Allocation(slot.coord, pool))))
          }
      }
    }
    solveRec(freeSlots, sortedServers, Solution(problem, Map.empty))
  }
}