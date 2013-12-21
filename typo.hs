module Typo where

import System.Random
import qualified Data.Vector as Vector
import qualified Data.Char   as Char

data Side = Left | Right deriving (Eq, Show)
data FingerName = Thumb | Index | Middle | Ring | Little deriving (Eq, Show)
data Finger = Finger FingerName Side deriving (Eq, Show)

allFingers :: [Finger]
allFingers = concat $
             map (\side -> map (\finger -> Finger finger side) fingers) sides
    where fingers = [Index, Middle, Ring, Little]
          sides   = [Typo.Left, Typo.Right]

pair :: [Finger] -> [[Finger]]
pair fingers = concat $ pair' fingers
    where pair' (f:[]) = []
          pair' (f:fs) = map (\f' -> [f, f']) fs : pair' fs

fingerToChars :: Finger -> String
fingerToChars f = case f of
                    Finger Index Typo.Left   -> "rfvcbgt"
                    Finger Middle Typo.Left  -> "edx"
                    Finger Ring Typo.Left    -> "wsz"
                    Finger Little Typo.Left  -> "qa<"
                    Finger Index Typo.Right  -> "yhnmjub"
                    Finger Middle Typo.Right -> "ik,"
                    Finger Ring Typo.Right   -> "ol."
                    Finger Little Typo.Right -> "pö-åä'-"
                    Finger Thumb _           -> ""

fingersToChars :: [[Finger]] -> [[String]]
fingersToChars fingers = map (\fl -> map fingerToChars fl) fingers

genWord :: Int -> String -> IO String
genWord maxWordLen chars =
    do len <- getStdRandom (randomR (1, maxWordLen))
       genWord' len (Vector.fromList chars) ""
    where
      genWord' :: Int -> Vector.Vector Char -> String -> IO String
      genWord' 0       _     res = do return res
      genWord' wordLen chars res =
          do idxR <- getStdRandom (randomR (0, Vector.length chars - 1))
             let c = chars Vector.! idxR
             genWord' (wordLen - 1) chars (c:res)

question :: Int -> Int -> [String] -> IO ()
question _     _    []             = do putStrLn "You are done!"
question limit 0    (chars:chars') = question limit limit chars'
question limit left s@(chars:chars') =
    do
      word <- genWord 10 chars
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
  let chars = map concat $ fingersToChars $ pair $ allFingers
  question 5 5 chars
