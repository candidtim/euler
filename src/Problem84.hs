{-# LANGUAGE ScopedTypeVariables #-}

{-
 - Solution for Problem 84 with simple simulation.
 - Simulates moves of a single token on the board and collects stop count per each cell.
 -}

module Problem84
  ( CellType(..)
  , Cell(..)
  , Board
  , board
  , findPosition
  , nextCellFromGroup
  , monopolyOddsToken
  ) where


import Text.Printf
import System.Random

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Tuple (swap, snd)

import Lib (shuffle)


-- BOARD DEFINITION

data CellType = Regular | Go2Jail | CommunityChest | Chance deriving (Show, Eq, Ord)

data Cell = Cell { name :: String
                 , cellType :: CellType
                 } deriving (Eq, Ord)

instance Show Cell where
  show = name

type Board = [Cell]

data Card = NeutralCard | Go2Pos Int | Go2NextR | Go2NextU | GoBack3 deriving (Show)
type CardDeck = [Card]


board :: Board
board =
  [ Cell "GO"   Regular
  , Cell "A1"   Regular
  , Cell "CC1"  CommunityChest
  , Cell "A2"   Regular
  , Cell "T1"   Regular
  , Cell "R1"   Regular
  , Cell "B1"   Regular
  , Cell "CH1"  Chance
  , Cell "B2"   Regular
  , Cell "B3"   Regular
  , Cell "JAIL" Regular
  , Cell "C1"   Regular
  , Cell "U1"   Regular
  , Cell "C2"   Regular
  , Cell "C3"   Regular
  , Cell "R2"   Regular
  , Cell "D1"   Regular
  , Cell "CC2"  CommunityChest
  , Cell "D2"   Regular
  , Cell "D3"   Regular
  , Cell "FP"   Regular
  , Cell "E1"   Regular
  , Cell "CH2"  Chance
  , Cell "E2"   Regular
  , Cell "E3"   Regular
  , Cell "R3"   Regular
  , Cell "F1"   Regular
  , Cell "F2"   Regular
  , Cell "U2"   Regular
  , Cell "F3"   Regular
  , Cell "G2J"  Go2Jail
  , Cell "G1"   Regular
  , Cell "G2"   Regular
  , Cell "CC3"  CommunityChest
  , Cell "G3"   Regular
  , Cell "R4"   Regular
  , Cell "CH3"  Chance
  , Cell "H1"   Regular
  , Cell "T2"   Regular
  , Cell "H2"   Regular
  ]

boardSize = length board

-- |Find position of a given cell name on the board
findPosition :: String -> Int
findPosition cs = fromJust $ L.findIndex (\c -> (name c) == cs) board

-- |Given a position on the board, provide a position of next cell of a given group
nextCellFromGroup :: Char -> Position -> Position
nextCellFromGroup c p =
  let indices = L.findIndices belongsToGroup board
      nextIndices = dropWhile (<=p) indices
   in if L.null nextIndices then head indices else head nextIndices
  where belongsToGroup (Cell (g:_) _) = c == g

-- Frequently used positions
posGo = findPosition "GO"
posJail = findPosition "JAIL"

communityChestDeck :: CardDeck
communityChestDeck = [Go2Pos posGo, Go2Pos posJail] ++ (take 14 $ repeat NeutralCard)

chanceDeck :: CardDeck
chanceDeck = [ Go2Pos posGo , Go2Pos posJail , Go2Pos (findPosition "C1") , Go2Pos (findPosition "E3")
             , Go2Pos (findPosition "H2") , Go2Pos (findPosition "R1")
             , Go2NextR, Go2NextR, Go2NextU, GoBack3 ] ++ (take 6 $ repeat NeutralCard)

-- |Number of cards in each deck
deckSize = 16

-- |Number of faces on the die (e.g. 6 for a classic cubic dice)
dieFaces = 4


-- SIMULATION MODEL

type Position = Int
type StopCount = Int

-- |Count of consecutive double dice rolled
type DoublesCount = Int

-- |CellStat represents statistics on stop count on every cell on the board
type CellStat = M.Map Cell StopCount

-- |Initial cell statistics with all cells having zero stop count
initCellStat :: CellStat
initCellStat = M.fromList $ map (\c -> (c, 0)) board

-- |Update cells statistics countin in a stop on a given cell
updatedStat :: CellStat -> Cell -> CellStat
updatedStat stat ncell = M.insert ncell (stat M.! ncell + 1) stat

rollDice :: StdGen -> (Int, Int, StdGen)
rollDice g =
  let (die1, g') = randomR (1, dieFaces) g
      (die2, g'') = randomR (1, dieFaces) g'
   in (die1, die2, g'')

randomCard :: StdGen -> CardDeck -> (Card, StdGen)
randomCard g cs =
  let (i, g') = randomR (0, deckSize-1) g
   in (cs !! i, g')


-- SIMULATION EXECUTION

-- |Provides a String token representing IDs of three most-probable cells on a Monopoly board. See
-- `https://projecteuler.net/problem=84` for details.
monopolyOddsToken :: StdGen -> Int -> String
monopolyOddsToken g n =
  let stat = monopolyCellStat g n
      sortedByStopCount = L.reverse $ L.sort $ map swap (M.toList stat)
      topCells = map snd sortedByStopCount
      topPositions = map (\c -> fromJust $ L.elemIndex c board) topCells
   in foldl (++) "" $ map (printf "%02d") $ take 3 topPositions

-- |Emulates moves of a single token on a board and collects statistics of getting to the cells of the board,
-- after having performed given number of moves.
monopolyCellStat :: StdGen -> Int -> CellStat
monopolyCellStat g n =
  let simulationStates = iterate moveOnce (0, 0, initCellStat, g)
      (_,_,finalStat,_) = simulationStates !! n
   in finalStat

-- |Core of the simulation logic: moves the virtual token once on the board in accordance to all rules
-- and provides new simulation state
moveOnce :: (DoublesCount, Position, CellStat, StdGen) -> (DoublesCount, Position, CellStat, StdGen)
moveOnce (doubles, p, stat, g) =
  let (die1, die2, g') = rollDice g
      newDoubles = if die1 == die2 then doubles + 1 else 0
      nextPos = if newDoubles == 3 then posJail else (p + die1 + die2) `mod` boardSize
      (finalPos, g'') = transition g' nextPos
      nextStat = updatedStat stat (board !! finalPos)
   in (newDoubles, finalPos, nextStat, g'')

-- |Moves the virtual token from given position to the final position according to transition rules:
-- special cells and cards if any apply in the given position; or keeps same position if no change
-- is applicable
transition :: StdGen -> Position -> (Position, StdGen)
transition g p = whilePosChanges g (p-1) p -- (p-1) is to shift the "previous" position compare to current
  where whilePosChanges g prevPos currPos
          | prevPos == currPos = (currPos, g)
          | otherwise = let (newPos, g') = jump g currPos (board !! currPos)
                         in whilePosChanges g' currPos newPos

jump :: StdGen -> Position -> Cell -> (Position, StdGen) -- FIXME: Maybe (Position,StdGen) ?
jump g _ Cell{cellType = Go2Jail} = (posJail, g)
jump g p Cell{cellType = CommunityChest} = jumpOnDeck g communityChestDeck p
jump g p Cell{cellType = Chance} = jumpOnDeck g chanceDeck p
jump g p _ = (p, g)

jumpOnDeck :: StdGen -> CardDeck -> Position -> (Position, StdGen)
jumpOnDeck g cs p =
  let (c, g') = randomCard g cs
   in (jumpOnCard c p, g')

-- |Considering that every card implies a position change (with Neutral card "changing" to same position),
-- apply the position change from a given one
jumpOnCard ::  Card -> Position -> Position
jumpOnCard (Go2Pos p) _ = p
jumpOnCard Go2NextR p = nextCellFromGroup 'R' p  -- FIXME: why can't I omit argument `p` here?
jumpOnCard Go2NextU p = nextCellFromGroup 'U' p
jumpOnCard GoBack3 p = p - 3
jumpOnCard NeutralCard p = p
