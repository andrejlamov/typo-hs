module Typo where

import System.Random
import qualified Data.Vector as Vector
import qualified Data.Char   as Char

data Side = Left | Right deriving (Eq, Show)
data FingerName = Thumb | Index | Middle | Ring | Little deriving (Eq, Show)
data Finger = Finger FingerName Side deriving (Eq, Show)

type Keys = String

allFingers :: [Finger]
allFingers = concat $
             map (\side -> map (\finger -> Finger finger side) fingers) sides
    where fingers = [Index, Middle, Ring, Little]
          sides   = [Typo.Left, Typo.Right]

pair :: [Finger] -> [[Finger]]
pair fingers = concat $ pair' fingers
    where pair' (f:[]) = []
          pair' (f:fs) = map (\f' -> [f, f']) fs : pair' fs

fingerToKeys :: Finger -> Keys
fingerToKeys f = case f of
                    Finger Index Typo.Left   -> "rfvcbgt"
                    Finger Middle Typo.Left  -> "edx"
                    Finger Ring Typo.Left    -> "wsz"
                    Finger Little Typo.Left  -> "qa<"
                    Finger Index Typo.Right  -> "yhnmjub"
                    Finger Middle Typo.Right -> "ik,"
                    Finger Ring Typo.Right   -> "ol."
                    Finger Little Typo.Right -> "pö-åä'-"
                    Finger Thumb _           -> ""

fingergroupsToKeys :: [[Finger]] -> [[Keys]]
fingergroupsToKeys fingers = map (\fl -> map fingerToKeys fl) fingers

genWord :: Int -> Int -> Keys -> IO Keys
genWord minWordLen maxWordLen chars =
    do maxWordLen' <- getStdRandom (randomR (minWordLen, maxWordLen))
       genWord' maxWordLen' (Vector.fromList chars) ""
    where
      genWord' :: Int -> Vector.Vector Char -> Keys -> IO Keys
      genWord' 0       _     res = do return res
      genWord' wordLen chars res =
          do idxR <- getStdRandom (randomR (0, Vector.length chars - 1))
             let c = chars Vector.! idxR
             genWord' (wordLen - 1) chars (c:res)

question :: Int -> Int -> [Keys] -> IO ()
question _     _    []             = do putStrLn "You are done!"
question limit 0    (chars:chars') = question limit limit chars'
question limit left s@(chars:chars') =
    do
      word <- genWord 5 10 chars
      question' word
      where
        question' word =
            do
              putStrLn $ "Type: " ++ word
              l <- getLine
              if l == word
              then question limit (left - 1) s
              else question' word

main:: IO ()
main = do
  let chars = map concat $ fingergroupsToKeys $ pair $ allFingers
  question 5 5 chars
