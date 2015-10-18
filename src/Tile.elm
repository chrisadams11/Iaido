---------------------------------TILE------------------------------------------
-------------------------------------------------------------------------------

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

getTile : List Tile -> Int -> Int -> Maybe Tile
getTile tiles row col =
  List.filter (\tile -> tile.position.row == row && tile.position.col == col) tiles
  |> List.head

isWalled : Tile -> {x: Int, y: Int} -> Bool
isWalled tile {x, y} =
  let
    isBlockedHorizontally = ((x == 1) && tile.walls.right) || ((x == -1) && tile.walls.left)
    isBlockedVertically = ((y == 1) && tile.walls.top) || ((y == -1) && tile.walls.bottom)
  in
    isBlockedVertically && isBlockedHorizontally
