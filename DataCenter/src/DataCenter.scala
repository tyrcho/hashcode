import DataCenterProblem.Problem
import DataCenterProblem.problem
import DataCenterProblem.Allocation
import DataCenterProblem.Server
import DataCenterProblem.Pool
import DataCenterProblem.FreeSlot
import DataCenterSolution.Solution

object DatacenterApp extends App {

  trait Solver {
    def solve(problem: Problem): Solution
  }

  object SequentialSolver extends Solver {
    def solve(problem: Problem): Solution = {
      val sortedServers = problem.servers.sortBy(_.ratio).reverse
      val freeSlots = problem.initialFreeSlot

      def solveRec(freeSlots: List[FreeSlot], servers: List[Server], pool: Pool, solution: Solution): Solution = {
        servers match {
          case Nil => solution
          case server :: t =>
            //TODO : sort by line
            freeSlots.find(_.size >= server.size) match {
              case None => solveRec(freeSlots, t, pool, solution.allocate(server, None))
              case Some(slot) =>
                val remainingSlot = slot.use(server.size)
                solveRec(
                  remainingSlot :: (freeSlots diff List(slot)),
                  t,
                  (pool + 1) % problem.nbPools,
                  solution.allocate(server, Some(Allocation(slot.coord, pool))))
            }
        }
      }
      solveRec(freeSlots, sortedServers, 0, Solution(Map.empty))
    }
  }
 
  val sol = SequentialSolver.solve(problem)
  sol.format.foreach(println)
  problem.validate(sol)
  println(problem.score(sol))
  
}