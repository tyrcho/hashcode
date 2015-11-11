package datacenter

import problem._

case class Solution(
    problem: Problem,
    serversAllocated: Map[Server, Option[Allocation]]) {

  def score: Int =
    (0 until problem.nbPools).map(scoreForPool).min

  def scoreForPool(pool: Pool): Int = {
    val totalCap = capacity(pool)
    val s = (0 until problem.nbRows).map(totalCap - capacityInRow(pool, _)).min
    s
  }

  def capacityInRow(pool: Pool, row: Int): Int = {
    (for {
      (server, allocOpt) <- serversAllocated
      alloc <- allocOpt
      if alloc.pool == pool
      if alloc.coord.row == row
    } yield server.capacity).sum
  }

  def capacity(pool: Pool): Int =
    (for {
      (server, allocOpt) <- serversAllocated
      alloc <- allocOpt
      if alloc.pool == pool
    } yield server.capacity).sum

  def validate() {
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
      case None                                => s"x\n"
      case Some(Allocation(Coord(r, s), pool)) => s"$r $s $pool\n"
    }
  }

  def debug: List[String] = for {
    row <- (0 until problem.nbRows).toList
  } yield debugRow(row)

  def debugRow(row: Int) = {
    val servers = for {
      (s, a) <- serversAllocated
      alloc <- a
      if alloc.coord.row == row
      slot = alloc.coord.slot
      id = s.id
      capa = s.capacity
      pool = alloc.pool
      size = s.size
    } yield slot -> s"$slot : [server $id in pool $pool capa=$capa size=$size]"
    val se = servers.toList.sortBy(_._1).map(_._2).mkString(" ")
    s"row $row : $se"
  }

  def debugPool(pool: Int) = {
    val servers = serversPerPool(pool).sortBy(_._2.coord.row)
    val maxCapa = servers.maxBy(_._1.capacity)
    val pools = poolsPerCapacity.toMap
    val capaPool = pools(pool)
    val score = scoreForPool(pool)

    val info = (for {
      (s, alloc) <- servers
      slot = alloc.coord.slot
      id = s.id
      capa = s.capacity
      pool = alloc.pool
      size = s.size
      row = alloc.coord.row
    } yield s"row $row : [server $id : capa=$capa size=$size]").mkString(" ")

    s"pool $pool with capa $capaPool and score $score ; max server : $maxCapa { $info }"
  }

  def capacity(pool: Pool, row: Int) = {
    for {
      (server, a) <- serversAllocated.toList
      alloc <- a
      if alloc.pool == pool
      if alloc.coord.row == row
    } yield server.capacity
  }.sum

  //capacity in case any other row fails
  def smartCapacity(pool: Pool, row: Int) = {
    val otherRows = (0 until problem.nbRows).toSet - row
    val byRows = for {
      r <- otherRows
    } yield capacity(pool, r)
    byRows.sum - byRows.max
  }

  def rowCapacity(row: Int) = {
    for {
      (server, a) <- serversAllocated.toList
      alloc <- a
      if alloc.coord.row == row
    } yield server.capacity
  }.sum

  def serversPerPool(pool: Pool) = {
    for {
      (server, a) <- serversAllocated.toList
      alloc <- a
      if alloc.pool == pool
    } yield (server, alloc)
  }

  def poolsPerCapacity = {
    for {
      i <- 0 until problem.nbPools
      servers = serversPerPool(i)
    } yield {
      val capaTotal = (for {
        (s, _) <- servers
        capa = s.capacity
      } yield capa).sum
      i -> capaTotal
    }
  }
}
