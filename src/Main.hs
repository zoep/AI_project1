{-# OPTIONS_GHC -O2 -optc-O2 #-}

import qualified Data.String as String
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import qualified AStar as AStar
import Data.Time
import System.Random

data Point = Point { x :: Int,
                     y :: Int }
    deriving (Show, Ord, Eq)

data Robot = Robot { position :: Point,
                     velocity :: Int }
    deriving (Show, Ord, Eq)

moveRandom grid robot = do
      let moves = movesRobot grid robot
      case Set.null moves of
          True  -> return Nothing
          False -> do 
                      index <- getStdRandom $ randomR (0, (Set.size moves) - 1)
                      return (Just ((Set.toList moves) List.!! index))

movesRobot grid robot = moveRobot robot Set.empty
  where moveRobot robot moves =
          let pos = (position robot) in
          let speed = (velocity robot) in 
          let moves1 = moveUp pos speed moves in
          let moves2 = moveDown pos speed moves1 in
          let moves3 = moveLeft pos speed moves2 in
          let moves4 = moveRight pos speed moves3 in
            moves4
        moveUp pos speed moves =
          let up = pos {y = (y pos) + speed} in
          validMove up moves
        moveDown pos speed moves =
          let down = pos {y = (y pos) - speed} in
          validMove down moves
        moveLeft pos speed moves =
          let left = pos {x = (x pos) - speed} in
          validMove left moves
        moveRight pos speed moves =
          let right = pos {x = (x pos) + speed} in
          validMove right moves
        validMove move moves =
          case Map.lookup move grid of
              Just 'O' -> Set.insert (robot { position = move}) moves
              otherwise -> moves

manhattanNorm target source = (abs $ (x target) - (x source)) + (abs $ (y target) - (y source))

main ::  IO ()
main = 
  do  str_grid <- getLine
      str_a <- getLine
      str_b <- getLine
      let Just grid_size = toPoint str_grid
      let Just a_pos = toPoint str_a
      let Just b_pos = toPoint str_b
      grid <- readGrid 0 (y grid_size) Map.empty
      let robot1 = Robot {position = a_pos, velocity = 1}
      let robot2 = Robot {position = b_pos, velocity = 1}
      start_time <- getCurrentTime
      (path, total) <- findRobot robot1 robot2 grid
      stop_time <- getCurrentTime
      case path of
            Nothing   -> putStr $ show "Robot1 could not meet with Robot2\n"
            Just path -> putStr $ show path ++ "\n" 
      putStr $ "nodes searched: " ++  (show total) ++ "\n"
      print $ diffUTCTime stop_time start_time
  where toPoint str = 
            case String.words str of
                  [a, b] -> Just Point { x = read a, y = read b}
                  _      -> Nothing 
        addPoint i (grid, j) p = (Map.insert (Point { x = i, y = j }) p grid, j+1)
        addLine line i grid = fst $ List.foldl (addPoint i) (grid, 0) line
        readGrid i y grid | i == y = do return grid
        readGrid i y grid =
           do str_line <- getLine
              readGrid (i+1) y (addLine str_line i grid)
        findPath [] acc = reverse acc
        findPath (x : xs) acc = findPath xs ((position x) : acc)
        findRobot robot1 robot2 grid =
          do
              (states, total) <- findRobot' robot1 robot2 [] 0
              case states of
                 Nothing     -> return (Nothing, total)
                 Just states -> return (Just (findPath states []), total)
           where findRobot' robot1 robot2 acc total = 
                    do
                        aMove <- moveRandom grid robot2 
                        case aMove of
                              Nothing    -> return (Nothing, total)
                              Just aMove -> 
                                      let robot2' = aMove in 
                                      let (robotStates, nr_nodes) = AStar.aStar (movesRobot grid) (\x y -> 1) (heuristic robot2') (samePosition robot2') robot1 in
                                        case robotStates of
                                              Nothing                         -> return (Nothing, nr_nodes)
                                              Just []                         -> return ((Just acc), total + nr_nodes)
                                              Just [step1]                    -> return (Just (step1 : acc), total + nr_nodes)
                                              Just [step1, step2]             -> return (Just (step2 : step1 : acc), total + nr_nodes)
                                              Just [step1, step2, step3]      -> return (Just (step3 : step2 : step1 : acc), total + nr_nodes)
                                              Just (step1 : step2 : step3 : xs) -> findRobot' step3 robot2' (step2 : step1 : acc) (nr_nodes + total)
                 samePosition robot robot' = (position robot) == (position robot')
                 heuristic target source = manhattanNorm (position target) (position source)

