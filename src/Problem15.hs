module Problem15
  ( latticePaths
  , nextGrid
  ) where


import Data.List as L

-- |Each node in grid represents number of possible paths from it to the target node in the grid
type Node = Int

-- |Grid is filled in with nodes indicating numbers of paths. Node opposite to target is
-- therefore indicating the number of lattice paths in this grid. In this case, Grid
-- represents nodes on the path, not the cells.
type Grid = [[Node]]

-- |Number of lattice paths in a grid of give size. Size is the number of cells on each side of the grid.
latticePaths :: Int -> Int
latticePaths gridSize =
  let grid = increaseGrid starterGrid (gridSize-1)
   in head.last $ grid

-- |Produces a grid bigger than given grid by a given number of nodes
increaseGrid :: Grid -> Int -> Grid
increaseGrid g 0 = g
increaseGrid g x = increaseGrid (nextGrid g) (x-1)

-- |Produces a grid of next size to a given one (e.g. 2x2 -> 3x3)
-- See test for an example
nextGrid :: Grid -> Grid
nextGrid xss =
  let xss' = foldl withNextColumn [] xss
      xs' = nextRow $ last xss'
   in xss' ++ [xs']

  where withNextColumn :: Grid -> [Node] -> Grid
        withNextColumn [] xs =  [ 1 : xs ]
        withNextColumn grid (x:xs) = let prevRow = last grid
                                         newRow = (head prevRow + x) : x : xs
                                      in grid ++ [newRow]

        nextRow :: [Node] -> [Node]
        nextRow xss = foldr nextElem [] xss

        nextElem 1 [] = [1]
        nextElem x xs = (x+(head xs)) : xs

-- |Minimal precalculated grid
starterGrid :: Grid
starterGrid = [ [ 1, 0 ]
              , [ 2, 1 ] ]
