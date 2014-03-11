-- Example of matrix multiply to start from
-- compile with 
-- % cabal build, or (ghc Main.hs -threaded -rtsopts -O2)
-- Run with
-- % ./dist/build/homework/homework -o report.html +RTS -K16M (or ./Main ...)
-- 

import GHC.Conc
import Criterion.Main
import System.Random
import Data.List


main = do
  let sz = 200
  m1 <- getRndMatrix sz sz
  m2 <- getRndMatrix sz sz
  defaultMain $
     [ bench "mm" (nf (mm m1) m2)
     ]


type Matrix = [[Double]]

mm :: Matrix -> Matrix -> Matrix
mm m1 m2 =
        [ [ sum $ zipWith (*) rows cols
          | cols <- transpose m2
          ]
          | rows <- m1 
        ]

-- get a random matrix of the right size
getRndMatrix :: Int -> Int -> IO Matrix
getRndMatrix m n = 
        sequence [ sequence [ getRnd | _ <- [1..n] ] | _ <- [1..m]]

getRnd :: IO Double
getRnd = do
  v <- randomIO
  s <- randomIO
  r <- randomIO
  return $ (if r || (v /= 0) then recip else id) 
         $ (if s then negate else id)
         $ v
         