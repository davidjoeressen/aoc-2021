{-# LANGUAGE TupleSections #-}
import Data.Maybe
import Data.List
import Data.Function
import System.Environment (getArgs)
import System.IO
import qualified Data.Map as M
import Data.Map (Map)

data Counts a = Counts { getCounts :: Map a Int } deriving Show

instance (Ord a) => Semigroup (Counts a) where
  (Counts x)<>(Counts y) = Counts $ M.unionWith (+) x y

instance (Ord a) => Monoid (Counts a) where
  mempty = Counts M.empty

type Substitutions = Map String Char

combineCounts :: (Ord a) => [(a,Int)] -> Counts a
combineCounts = Counts . M.fromListWith (+)

prepareStart :: String -> (Counts Char, Counts String)
prepareStart xs = (count $ init $ tail xs, count $ chunk xs)

count :: (Ord a) => [a] -> Counts a
count = combineCounts . map (,1)

parse :: String -> (String, Char)
parse x = (take 2 x,last x)

chunk :: [a] -> [[a]]
chunk xs
  | length xs > 1 = take 2 xs:chunk (tail xs)
  | otherwise = []

lookup' s = fromJust . flip M.lookup s

applyOne :: Substitutions -> (String,Int) -> Counts String
applyOne s (x@[a,b],n) = let c = lookup' s x in combineCounts $ map (,n) [[a,c],[c,b]]

apply :: Substitutions -> Counts String -> (Counts Char,Counts String)
apply s (Counts cs) = (charcount, strcount)
  where strcount = mconcat $ map (applyOne s) $ M.assocs cs
        charcount = Counts $ M.mapKeysWith (+) (lookup' s) cs

expand :: (Ord a) => Counts [a] -> Counts a
expand (Counts xs) = Counts (M.mapKeysWith (+) head xs) <> Counts (M.mapKeysWith (+) last xs)

calc :: (Counts Char,Counts String) -> Int
calc (Counts xs, ys) = maximum counts - minimum counts
  where counts = M.elems $ getCounts $ Counts (fmap (*(-1)) xs)<>expand ys

main = do
  file <- head <$> getArgs
  handle <- openFile file ReadMode
  start <- prepareStart <$> hGetLine handle
  hGetLine handle
  subst <- M.fromList . map parse . lines <$> hGetContents handle
  let [part1,part2] = map calc $ flip map [10,40] $ (!!) $ iterate (apply subst =<<) start
  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2
