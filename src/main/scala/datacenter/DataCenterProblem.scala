package datacenter

package object problem {

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

    def initialFreeSlot: List[FreeSlot] = {
      for {
        r <- (0 until nbRows).toList
        unavailable = -1 :: unavailableSlots.filter(_.coord.row == r).sortBy(_.coord.slot).map(_.coord.slot) ::: List(nbSlotsPerRow)
        List(begin, end) <- unavailable.tails.filter(_.size >= 2).map(_.take(2))
        size = end - begin - 1
        if size > 0
      } yield FreeSlot(Coord(r, begin + 1), size)
    }

  }

}