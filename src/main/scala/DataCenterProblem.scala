import DataCenterSolution.Solution

package object DataCenterProblem {

  case class Coord(row: Int, slot: Int)

  type Pool = Int
  case class Allocation(coord: Coord, pool: Pool)

  case class FreeSlot(coord: Coord, size: Int) {
    def use(capacity: Int) = FreeSlot(Coord(coord.row, coord.slot + capacity), size - capacity)
  }
  
  case class UnavailableSlot(coord: Coord)
  
   case class Server(size: Int, capacity: Int, id: Int) {
    def ratio: Float = capacity / size.toFloat
  }

  case class Problem(nbRows: Int,
                     nbSlotsPerRow: Int,
                     servers: List[Server],
                     unavailableSlots: List[UnavailableSlot],
                     nbPools: Int) {

    def score(solution: Solution): Int = {
      def capacity(pool: Pool): Int =
        (for {
          (server, allocOpt) <- solution.serversAllocated
          alloc <- allocOpt
          if alloc.pool == pool
        } yield server.capacity).sum

      def capacityInRow(pool: Pool, row: Int): Int = {
        (for {
          (server, allocOpt) <- solution.serversAllocated
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

    def initialFreeSlot: List[FreeSlot] = {
      for {
        r <- (0 until nbRows).toList
        unavailable = -1 :: unavailableSlots.filter(_.coord.row == r).sortBy(_.coord.slot).map(_.coord.slot) ::: List(nbSlotsPerRow)
        List(begin, end) <- unavailable.tails.filter(_.size >= 2).map(_.take(2))
        size = end - begin - 1
        if size > 0
      } yield FreeSlot(Coord(r, begin + 1), size)
    }

    def validate(solution: Solution): Unit = {
      val allocatedArray = for {
        (server, optAlloc) <- solution.serversAllocated.toList
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

  }

  val input = io.Source.fromFile("input/data.in").getLines.toList
  val first = input.head
  val Array(nbRows, nbSlots, nbUnavailable, nbPools, nbServers) = first.split(" ").map(_.toInt)

  val servers = for {
    (line, i) <- input.tail.drop(nbUnavailable).take(nbServers).zipWithIndex
    Array(s, c) = line.split(" ")
  } yield Server(s.toInt, c.toInt, i)

  val allAllocated = Solution((for {
    s <- servers
  } yield s -> Some(Allocation(Coord(0, 0), 1))).toMap)
  val allNotAllocated = Solution((for {
    s <- servers
  } yield s -> None).toMap)

  val unavailable = for {
    line <- input.tail.take(nbUnavailable)
    Array(r, s) = line.split(" ")
  } yield UnavailableSlot(Coord(r.toInt, s.toInt))

  val problem = Problem(nbRows, nbSlots, servers, unavailable, nbPools)

}