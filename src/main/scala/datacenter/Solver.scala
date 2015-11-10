package datacenter

import problem._

trait Solver {
  def solve(problem: Problem): Solution
}

object SequentialSolver extends Solver {
  def solve(problem: Problem): Solution = {
    val sortedServers = problem.servers.sortBy(s => s.ratio + s.size).reverse
    val freeSlots = problem.initialFreeSlot

    def solveRec(freeSlots: List[FreeSlot], servers: List[Server], solution: Solution): Solution = {
      servers match {
        case Nil => solution
        case server :: t =>
          val pools = solution.poolsPerCapacity(problem.nbPools)
          val pool = pools.minBy(_._2)._1 // lowest capacity pool
          val rowsByCapacityForPool = (0 until problem.nbRows).sortBy { row =>
            solution.capacity(pool, row) + solution.rowCapacity(row)
          }
          val slots = freeSlots.sortBy(slot => rowsByCapacityForPool.indexOf(slot.coord.row))
          slots.find(_.size >= server.size) match {
            case None => solveRec(freeSlots, t, solution.allocate(server, None))
            case Some(slot) =>
              val remainingSlot = slot.use(server.size)
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