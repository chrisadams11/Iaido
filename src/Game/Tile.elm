-------------------------------------------------------------------------------
---------------------------------TILE------------------------------------------
-------------------------------------------------------------------------------
-- This section defines the Tile actor, which represents one space on the
-- two-dimensional board. Tiles are uniquely identified by their row and
-- column position, and may be bordered by walls on any of their four sides.
------------------------------------------------------------------------------

-- Definition of the Tile actor.
type alias Tile =
  {
    position: {row: Int, col: Int},
    walls:
      {
        left: Bool,
        right: Bool,
        top: Bool,
        bottom: Bool
      }
  }


-- Retrieves a tile actor given its row and column
getTile : List Tile -> Int -> Int -> Maybe Tile
getTile tiles row col =
  List.filter (\tile -> tile.position.row == row && tile.position.col == col) tiles
  |> List.head

-- Checks if a tile blocks movement from a specified direction.
-- In the case of diagonal movement, both directions must be walled for a
-- block to occur.
-- The specified direction is given as a vector that describes the direction
-- of movement through the tile, originating from the center.
-- e.g., (0,1) is moving directly up through  the roof of the tile,
-- and (-1, 0) is moving directly left.
isWalled : Tile -> {x: Int, y: Int} -> Bool
isWalled tile {x, y} =
  let
    clearHorizontally = ((x == 1) && not tile.walls.right) || ((x == -1) && not tile.walls.left) || (x == 0)
    clearVertically = ((y == 1) && not tile.walls.top) || ((y == -1) && not tile.walls.bottom) || (y == 0)
  in
    not (clearHorizontally || clearVertically)


--From a list of tiles, returns all tiles that appear more than once in the list.
findRepeatTiles: List Tile -> List Tile
findRepeatTiles l =
  if l == [] then []
  else
    let
      sortedList = List.sortWith compareTiles l
    in
      (List.foldl
        (\elem (previousElem, acc) ->
          if (elem == previousElem)
          then (elem, elem :: acc)
          else (elem, acc))
        (unsafeHead sortedList, [])
        (unsafeTail sortedList)) |> snd


-- Simple comparison of tiles, by their row and column position.
compareTiles: Tile -> Tile -> Order
compareTiles l r =
  if l.position.row > r.position.row then GT
  else if l.position.row < r.position.row then LT
  else
    if l.position.col > r.position.col then GT
    else if l.position.col < r.position.col then LT
    else
      EQ
