{-# LANGUAGE TupleSections #-}
import Data.Maybe
import Data.List
import Data.Function
import System.Environment (getArgs)
import System.IO

data Counts a = Counts { getCounts :: [(a,Int)] } deriving Show

instance (Ord a) => Semigroup (Counts a) where
  (Counts x)<>(Counts y) = Counts $ combineCounts (x++y)

instance (Ord a) => Monoid (Counts a) where
  mempty = Counts []

combineCounts :: (Ord a) => [(a,Int)] -> [(a,Int)]
combineCounts = map ((,)<$>fst.head<*>sum.map snd) . groupBy ((==)`on`fst) . sort

prepareStart :: String -> (Counts Char, Counts String)
prepareStart xs = (count $ init $ tail xs, count $ chunk xs)

count :: (Ord a) => [a] -> Counts a
count = Counts . map ((,)<$>head<*>length) . group . sort

type Substitution = (String, ([String],Char))

parse :: String -> Substitution
parse x = (y,([[a,z],[z,b]],z))
  where y@[a,b] = take 2 x
        z = last x

chunk :: [a] -> [[a]]
chunk xs
  | length xs > 1 = take 2 xs:chunk (tail xs)
  | otherwise = []

lookup' x = fromJust . lookup x

applyOne :: [Substitution] -> (String,Int) -> ((Char,Int),[(String,Int)])
applyOne s (x,n) = ((c,n),map (,n) xs)
  where (xs,c) = lookup' x s

apply :: [Substitution] -> Counts String -> (Counts Char,Counts String)
apply s (Counts cs) = (Counts charcount, Counts $ combineCounts strcount)
  where (charcount, strcount) = fmap concat $ unzip $ map (applyOne s) cs

expand :: (Ord a) => [([a],Int)] -> [(a,Int)]
expand = combineCounts . concatMap (\([a,b],n) -> [(a,n),(b,n)])

calc :: (Counts Char,Counts String) -> Int
calc (Counts cs, Counts ss) = maximum cs' - minimum cs'
  where cs' = map snd $ combineCounts (map (fmap (*(-1))) cs++expand ss)

main = do
  file <- head<$>getArgs
  handle <- openFile file ReadMode
  start <- prepareStart <$> hGetLine handle
  hGetLine handle
  subst <- map parse . lines <$> hGetContents handle
  let [part1,part2] = map calc $ flip map [10,40] $ (!!) $ iterate (apply subst =<<) start
  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2
