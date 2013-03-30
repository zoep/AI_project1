import qualified Data.String as String
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import qualified AStar as AStar
import Data.Time
import System.Random
import Debug.Trace

data Point = Point { x :: Int,
                     y :: Int }
    deriving (Show, Ord, Eq)

data Robot = Robot { position :: Point,
                     velocity :: Int }
    deriving (Show, Ord, Eq)

moveRandom ::  Map.Map Point Char -> Robot -> IO (Maybe Robot)
moveRandom grid robot = do
      let moves = movesRobot grid robot
      case Set.null moves of
          True  -> return Nothing
          False -> do 
                      index <- getStdRandom $ randomR (0, (Set.size moves) - 1)
                      return (Just ((Set.toList moves) List.!! index))

movesRobot ::  Map.Map Point Char -> Robot -> Set.Set Robot
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

manhattanNorm ::  Point -> Point -> Int
manhattanNorm target source = (abs $ (x target) - (x source)) + (abs $ (y target) - (y source))

euclideanNorm ::  Integral b => Point -> Point -> b
euclideanNorm target source = floor $ sqrt (fromIntegral (a*a + b*b))
  where
       a = (x target) - (x source)
       b = (y target) - (y source)

heuristic target source = manhattanNorm (position target) (position source)


main ::  IO ()
main = 
  do  str_grid <- getLine
      str_a <- getLine
      str_b <- getLine
      let Just grid_size = toPoint str_grid
      let Just a_pos = toPoint str_a
      let Just b_pos = toPoint str_b
      grid <- readGrid 1 (y grid_size) Map.empty
      let robot1 = Robot {position = a_pos, velocity = 1}
      let robot2 = Robot {position = b_pos, velocity = 1}
      let (reachable, _) = AStar.aStar (movesRobot grid) (\x y -> 1) (heuristic robot2) (samePosition robot2) robot1
      putStr $ "Initial positions: Robot1 " ++ (show (position robot1)) ++ " Robot2 " ++ (show (position robot2)) ++ "\n"   
      case reachable of
               Nothing -> putStr "Robot1 could not meet with Robot2\n"
               Just _ ->
                 do
                    start_time <- getCurrentTime
                    (r1, r2, path, total) <- findRobot robot1 robot2 grid
                    stop_time <- getCurrentTime
                    case path of
                       Nothing   -> putStr $ show "Robot1 could not meet with Robot2. Internal error.\n"
                       Just path -> putStr $ "Final positions: Robot1 " ++ (show (position r1)) ++ " Robot2 " ++ (show (position r2)) ++ "\n" ++
                                             "nodes searched: " ++  (show total) ++ " Path: " ++ (show (List.length path)) ++ "\n"
                    print $ diffUTCTime stop_time start_time
  where toPoint str = 
            case String.words str of
                  [a, b] -> Just Point { x = read a, y = read b}
                  _      -> Nothing 
        addPoint i (grid, j) p = (Map.insert (Point { x = j, y = i }) p grid, j+1)
        addLine line i grid = fst $ List.foldl (addPoint i) (grid, 1) line
        readGrid i y grid | i > y = do return grid
        readGrid i y grid =
           do str_line <- getLine
              readGrid (i+1) y (addLine str_line i grid)
        findPath [] acc = acc
        findPath (x : xs) acc = findPath xs ((position x) : acc)
        samePosition robot robot' = (position robot) == (position robot')
        findRobot robot1 robot2 grid =
          do
              (r1, r2, states, total) <- findRobot' robot1 robot2 [robot1] 0
              case states of
                 Nothing     -> return (r1, r2, Nothing, total)
                 Just states -> return (r1, r2, Just (findPath states []), total)
          where findRobot' robot1 robot2 acc total = 
                  do
                      aMove <- moveRandom grid robot2 
                      case aMove of
                            Nothing    -> return (robot1, robot2, Nothing, total)
                            Just aMove ->
                                 let robot2' = aMove in 
                                 let (robotStates, nr_nodes) = AStar.aStar (movesRobot grid) (\x y -> 1) (heuristic robot2') (samePosition robot2') robot1 in
                                 case robotStates of
                                         Nothing                         -> return (robot1, robot2', Nothing, nr_nodes)
                                         Just []                         -> return (robot1, robot2', (Just acc), total + nr_nodes)
                                         Just [step1]                    -> return (step1, robot2', Just (step1 : acc), total + nr_nodes)
                                         Just [step1, step2]             -> return (step2, robot2', Just (step2 : step1 : acc), total + nr_nodes)
                                         Just [step1, step2, step3]      -> return (step3, robot2', Just (step3 : step2 : step1 : acc), total + nr_nodes)
                                         Just (step1 : step2 : step3 : xs) -> findRobot' step3 robot2' (step3 : step2 : step1 : acc) (nr_nodes + total)
                
                


