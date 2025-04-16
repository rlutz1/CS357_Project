module Main where

import Brillo
import Brillo.Interface.IO.Game
import Board

main :: IO ()
main = 
  do
    let window = mainWindow
    let world = getWorld
    play 
      window
      white
      100
      world
      drawFunc
      handleEvent            
      updateFunc
    -- play 
    --   window
    --   white
    --   100
    --   world
    --   -- (\world -> translate 50 50 world)
    --   id
    --   handleEvent            
    --   (\_ cir -> cir)

    -- animate 
    --   window
    --   white 
    --   (\t -> getWorld t)

    -- simulate
    --   window 
    --   white
    --   100
    --   getWorld
    --   id
    --   (\vp x m -> m)

updateFunc :: Float -> World -> World
updateFunc t (World (Player (x, y) dir next (vx, vy)) b)
  | dir /= next = move (World (Player (x, y) dir next (vx, vy)) b) next t
  | otherwise = move (World (Player (x, y) dir next (vx, vy)) b) dir t

drawFunc :: World -> Picture
drawFunc world = Pictures (drawGame world)

drawGame :: World -> [Picture]
drawGame (World p b) = drawPlayer p : drawBoard b
-- getNodePics w = drawPlayer w : genCircles (getPoints w)

drawPlayer :: Player -> Picture -- todo no offset for now
drawPlayer (Player (x, y) _ _ _) = Color black (translate a b (Circle 15))
 where
    a = x * 100 - 150 -- off set not used right now
    b = y * 100 - 150

drawBoard :: Board -> [Picture]
drawBoard (Board []) = [] 
drawBoard (Board (t:ts)) = drawTile t : drawBoard (Board (ts))

drawTile :: Tile -> Picture
drawTile (Tile ((Zone xStart xEnd1 yStart yEnd1), Wall)) = Color blue (translate a1 b1 (Circle 20))
  where
      a1 = xEnd1 * 100 - 150 -- off set not used right now
      b1 = yEnd1 * 100 - 150
drawTile (Tile ((Zone xStart xEnd2 yStart yEnd2), _)) = Color red (translate a2 b2 (Circle 20))
  where
    a2 = xEnd2 * 100 - 150 -- off set not used right now
    b2 = yEnd2 * 100 - 150

genCircles :: [(Int, Int)] -> [Picture]
genCircles [] = []
genCircles ((x, y):pts) = Color red (translate a b (Circle 20)) : genCircles pts
  where
    a = fromIntegral (x * 100 - 150)
    b = fromIntegral (y * 100 - 150)

-- getPoints :: World -> [(Int, Int)]
-- getPoints (World _ _ _ (Grid points)) = points

data Grid = Grid [(Int, Int)]
  deriving (Show)

data World = World {
  player :: Player,
  board :: Board
}
  deriving (Show)

data Player = Player {
  playerLocation :: (Float, Float),
  currDirection :: Direction, 
  nextDirection :: Direction, 
  velocity :: (Float, Float)
}
  deriving (Show)

-- data Direction = UP | DOWN | LEFT | RIGHT | NONE
--   deriving (Enum, Eq, Show)

getWorld :: World
getWorld = World  (Player (0, 0) NONE NONE (0, 0)) (genLevel 1)

-- getGrid :: Int -> Int -> [(Int, Int)]
-- getGrid l w = [(x, y) | x <- [0..l], y <- [0..w]]

handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) w = tryMove w UP
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) w = tryMove w DOWN
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) w = tryMove w LEFT
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) w = tryMove w RIGHT
handleEvent _ w = w

tryMove :: World -> Direction -> World
tryMove (World (Player pos curr next v) b) dir =   
  (World (Player pos curr dir v) b)

move :: World -> Direction -> Float -> World
move (World (Player (x, y) curr next (vx, vy)) b) dir t =
  (World (Player (dx, dy) dir next (getVelocity dir)) b)
  where 
    dx = x + vx * t
    dy = y + vy * t

getVelocity :: Direction -> (Float, Float)
getVelocity dir
  | dir == UP    = (0.0,  1.0)
  | dir == DOWN  = (0.0, -1.0)
  | dir == LEFT  = (-1.0, 0.0)
  | dir == RIGHT = ( 1.0, 0.0)
  | otherwise = (0, 0)

specialKeyPressed :: Event -> Bool
specialKeyPressed (EventKey (Char k) _ _ _) = k == 'g'
specialKeyPressed _ = False

mainWindow :: Display
mainWindow = InWindow "Nice Window" (700, 700) (10,10)
