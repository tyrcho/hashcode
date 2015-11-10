import DataCenterProblem._

package object DataCenterSolution {

  case class Solution(
      serversAllocated: Map[Server, Option[Allocation]]) {

    def allocate(server: Server, allocation: Option[Allocation]) =
      copy(serversAllocated = serversAllocated + (server -> allocation))

    def format: List[String] = for {
      (server, optAlloc) <- serversAllocated.toList.sortBy(_._1.id)
    } yield {
      optAlloc match {
        case None                                => s"$server : x"
        case Some(Allocation(Coord(r, s), pool)) => s"$server : $r $s $pool"
      }
    }

    def capacity(pool: Pool, row: Int) = {
      for {
        (server, a) <- serversAllocated.toList
        alloc <- a
        if alloc.pool == pool
        if alloc.coord.row == row
      } yield server.capacity
    }.sum

    def serversPerPool(pool: Pool): List[Server] = {
      for {
        (server, a) <- serversAllocated.toList
        alloc <- a
        if alloc.pool == pool
      } yield server
    }

    def poolsPerCapacity(maxPools: Int) = {
      for {
        i <- 0 until maxPools
        servers = serversPerPool(i)
      } yield {
        val capaTotal = (for {
          s <- servers
          capa = s.capacity
        } yield capa).sum
        i -> capaTotal
      }
    }
  }

}