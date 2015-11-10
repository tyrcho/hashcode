import DataCenterProblem.Server
import DataCenterProblem.Allocation
import DataCenterProblem.Coord

package object DataCenterSolution {

  case class Solution(
      serversAllocated: Map[Server, Option[Allocation]]) {

    def allocate(server: Server, allocation: Option[Allocation]) =
      copy(serversAllocated = serversAllocated + (server -> allocation))

    def format: List[String] = for {
      (server, optAlloc) <- serversAllocated.toList.sortBy(_._1.id)
    } yield {
      optAlloc match {
        case None                                => "x"
        case Some(Allocation(Coord(r, s), pool)) => s"$r $s $pool"
      }
    }

  }

}