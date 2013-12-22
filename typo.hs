module Typo where

import System.Random
import qualified Data.Vector as Vector
import qualified Data.Char   as Char

data Side = Left | Right deriving (Eq, Show)
data FingerName = Thumb | Index | Middle | Ring | Little deriving (Eq, Show)
data Finger = Finger FingerName Side deriving (Eq, Show)

type Key  = Char
type Keys = [Key]

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
genWord minWordLen maxWordLen keys =
    do maxWordLen' <- getStdRandom (randomR (minWordLen, maxWordLen))
       genWord' maxWordLen' (Vector.fromList keys) ""
    where
      genWord' :: Int -> Vector.Vector Key -> Keys -> IO Keys
      genWord' 0       _    res = do return res
      genWord' wordLen keys res =
          do idxR <- getStdRandom (randomR (0, Vector.length keys - 1))
             let k = keys Vector.! idxR
             genWord' (wordLen - 1) keys (k:res)

question :: Int -> Int -> [Keys] -> IO ()
question _     _    []             = do putStrLn "You are done!"
question limit 0    (keys:keys') = question limit limit keys'
question limit left keygroups@(keys:keys') =
    do
      word <- genWord 5 10 keys
      question' word
      where
        question' word =
            do
              putStrLn $ "Type: " ++ word
              l <- getLine
              if l == word
              then question  limit (left - 1) keygroups
              else question' word

main:: IO ()
main = do
  let keys = map concat $ fingergroupsToKeys $ pair $ allFingers
  question 5 5 keys




