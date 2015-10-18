--The tile collection is a data structure used to store the game tiles
--It is implemented as a one-dimensional array that mimics a two-dimensional
--array.

type alias TileCollection = Array Tile

--Constructs a rectangle of tesselated row by col tiles.
buildTileGrid : Int -> Int -> TileCollection -> TileCollection
buildTileGrid rows cols tc = if cols == 0 then tc
                              else (buildTileGrid rows (cols-1) tc) `Array.append` (newColumn rows cols)

--Constructs a column of n tiles starting at x position
newColumn : Int -> Int -> Array Tile
newColumn rows col = Array.initialize rows (\i -> newTile col (i+1))

--Constructs a new at a given position
newTile : Int -> Int -> Tile
newTile col row = (tile
                    (col*1000 + row)
                    ((tileRadius * (toFloat col) * 1.55) - halfWidth)
                    ((tileRadius * (toFloat row) * 1.7) + (tileRadius * 0.85 * (toFloat (col%2))) - halfHeight)
                  )

--Retrieves a tile by ID from the collection
getTile : TileCollection -> TileID -> Tile
getTile tileCollection tileID =
  let colIndex = round ((toFloat tileID) / 1000)
      rowIndex = (tileID % 1000)
  in tileCollection
      |> getTileAt rowIndex colIndex
      |> withDefault defaultTile

--Retrieves a tile by grid coordinate from the collection
getTileAt : Int -> Int -> TileCollection -> Maybe Tile
getTileAt row col tileCollection = if (row > tileRows) || (row < 1) then Nothing else
  Array.get (((col-1)*tileRows) + (row-1)) tileCollection

--Retrieves a tile by position from the collection
getTileNear : Float -> Float -> TileCollection -> Maybe Tile
getTileNear x y tileCollection = tileCollection
  |> Array.filter (\tile -> inTile tile (x,y))
  |> Array.get 0

--Updates a tile in the collection
setTile : TileCollection -> Tile -> TileCollection
setTile tileCollection tile =
  let colIndex = round ((toFloat tile.tileID) / 1000)
      rowIndex = (tile.tileID % 1000)
  in tileCollection
      |> setTileAt rowIndex colIndex tile

--Updates a tile in the collection based on grid position
setTileAt : Int -> Int -> Tile -> TileCollection -> TileCollection
setTileAt row col tile tileCollection = if (row > tileRows) then tileCollection else
  Array.set (((col-1)*tileRows) + (row-1)) tile tileCollection

--Retrieves a list of tiles adjacent to the given tile
adjacentTiles : TileCollection -> TileID -> List Tile
adjacentTiles tileCollection tileID =
  let colIndex = round ((toFloat tileID) / 1000)
      rowIndex = (tileID % 1000)
  in
    (case (getTileAt (rowIndex - 1) colIndex tileCollection) of
      Just t -> [t]
      Nothing -> [])
    ++
    (case (getTileAt (rowIndex + 1) colIndex tileCollection) of
      Just t -> [t]
      Nothing -> [])
    ++
    (case (getTileAt rowIndex (colIndex - 1) tileCollection) of
      Just t -> [t]
      Nothing -> [])
    ++
    (case (getTileAt rowIndex (colIndex + 1) tileCollection) of
      Just t -> [t]
      Nothing -> [])
    ++
    (if (colIndex % 2) == 0
      then
        (case (getTileAt (rowIndex - 1) (colIndex - 1) tileCollection) of
          Just t -> [t]
          Nothing -> [])
        ++
        (case (getTileAt (rowIndex - 1) (colIndex + 1) tileCollection) of
          Just t -> [t]
          Nothing -> [])
      else
        (case (getTileAt (rowIndex + 1) (colIndex - 1) tileCollection) of
          Just t -> [t]
          Nothing -> [])
        ++
        (case (getTileAt (rowIndex + 1) (colIndex + 1) tileCollection) of
          Just t -> [t]
          Nothing -> []))

--Retrieves the IDs of tiles adjacent to this one
adjacentTileIDs : TileCollection -> TileID -> List TileID
adjacentTileIDs tileCollection tileID =
  (adjacentTiles tileCollection tileID)
  |> List.map (\tile -> tile.tileID)

--Estimates the distance between two tiles,
--used as a heuristic in A*
estimateDistance : TileCollection -> TileID -> TileID -> Int
estimateDistance tileCollection a b =
  let from = (getTile tileCollection a)
      to = (getTile tileCollection b)
  in
    round (vectDistance (from.x, from.y) (to.x, to.y))

--Performs a map from Tile to Tile over the tile collection
tileMap : (Tile -> Tile) -> TileCollection -> TileCollection
tileMap f tileCollection = Array.map f tileCollection

--Performs a map over the tile collection
--returns a list
tileMapFlat : (Tile -> a) -> TileCollection -> List a
tileMapFlat f tileCollection = tileCollection
  |> Array.toList
  |> List.map f
