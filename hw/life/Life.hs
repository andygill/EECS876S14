{- Game of life example from section 9.7 of Programming in Haskell,
   Graham Hutton, Cambridge University Press, 2007.

   Edited by Andy Gill for EECS 876.
    - using thread delay
    - using standard combinators like sequence and nub
  -}

import Control.Concurrent
import Data.List

-- Derived primitives

cls                           :: IO ()
cls                           =  putStr "\ESC[2J"

type Pos                      = (Int,Int)

goto                          :: Pos -> IO ()
goto (x,y)                    =  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat                       :: Pos -> String -> IO ()
writeat p xs                  =  do goto p
                                    putStr xs

-- Game of life

width                         :: Int
width                         =  20

height                        :: Int
height                        =  20

type Board                    =  [Pos]

glider                        :: Board
glider                        =  [(4,2),(2,3),(4,3),(3,4),(4,4)]

showcells                     :: Board -> IO ()
showcells b                   =  sequence_ [writeat p "O" | p <- b]

isAlive                       :: Board -> Pos -> Bool
isAlive b p                   =  elem p b

isEmpty                       :: Board -> Pos -> Bool
isEmpty b p                   =  not (isAlive b p)

neighbs                       :: Pos -> [Pos]
neighbs (x,y)                 =  map wrap [(x-1,y-1), (x,y-1),
                                           (x+1,y-1), (x-1,y),
                                           (x+1,y)  , (x-1,y+1),
                                           (x,y+1)  , (x+1,y+1)] 

wrap                          :: Pos -> Pos
wrap (x,y)                    =  (((x-1) `mod` width) + 1, ((y-1) `mod` height + 1))

liveneighbs                   :: Board -> Pos -> Int
liveneighbs b                 =  length . filter (isAlive b) . neighbs

survivors                     :: Board -> [Pos]
survivors b                   =  [p | p <- b, elem (liveneighbs b p) [2,3]]

births b                      =  [p | p <- nub (concat (map neighbs b)),
                                      isEmpty b p,
                                      liveneighbs b p == 3]

nextgen                       :: Board -> Board
nextgen b                     =  survivors b ++ births b

life                          :: Board -> IO ()
life b                        =  do cls
                                    showcells b
                                    threadDelay (50 * 1000)
                                    life (nextgen b)

