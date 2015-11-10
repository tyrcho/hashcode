package datacenter

import problem._

case class Solution(
    serversAllocated: Map[Server, Option[Allocation]]) {

  def score(problem: Problem): Int = {
    import problem._

    def capacity(pool: Pool): Int =
      (for {
        (server, allocOpt) <- serversAllocated
        alloc <- allocOpt
        if alloc.pool == pool
      } yield server.capacity).sum

    def capacityInRow(pool: Pool, row: Int): Int = {
      (for {
        (server, allocOpt) <- serversAllocated
        alloc <- allocOpt
        if alloc.pool == pool
        if alloc.coord.row == row
      } yield server.capacity).sum
    }

    def scoreForPool(pool: Pool): Int = {
      val totalCap = capacity(pool)
      (0 until nbRows).map(totalCap - capacityInRow(pool, _)).min
    }

    (0 until nbPools).map(scoreForPool).min

  }

  def validate(problem: Problem) {
    import problem._

    val allocatedArray = for {
      (server, optAlloc) <- serversAllocated.toList
      Allocation(Coord(row, slot), _) <- optAlloc.toList
      size = server.size
      s <- slot until (slot + size)
    } yield Coord(row, s)

    def validateDuplicateServerSlots(): Unit = {
      val unavailableArray = unavailableSlots.map(_.coord)
      val allUnavailable = allocatedArray ::: unavailableArray
      assert(allUnavailable.distinct.size == allUnavailable.size)
    }

    def validateBoundsServerSlots(): Unit = {
      for { Coord(row, slot) <- allocatedArray }
        assert(row >= 0 && row < nbRows && slot >= 0 && slot < nbSlotsPerRow)
    }

    validateDuplicateServerSlots()
    validateBoundsServerSlots()
  }

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
