module Main where

data Color = Red | Blue | Empty | Broken
 deriving(Eq, Show)

type Set = Int -> Bool
type Graph = Int -> Set
singletonSet :: Graph
singletonSet x y = x == y
setAdd :: Set -> Int -> Set
setAdd set x el = set el || x == el
setToList :: Set -> Int -> [Int]
setToList set limit = filter set [0..limit]
emptySet :: Set
emptySet _ = False
emptyGraph :: Graph
emptyGraph _ = emptySet
invertColor :: Color -> Color
invertColor color
 | color == Red = Blue
 | color == Blue = Red
 | otherwise = Empty
type ColorProvider = Int -> Color

emptyColorProvider :: ColorProvider
emptyColorProvider _ = Empty
-- Добавление ребра.
addEdge :: Graph -> (Int, Int) -> Graph
addEdge graph (a, b) v =  case v of
  _ | v == a -> setAdd (graph v) b
    | v == b -> setAdd (graph v) a
    | otherwise -> graph v
-- Окрашивание вершины.
setColor :: ColorProvider -> (Int, Color) -> ColorProvider
setColor cp (p, color)
  | (color /= Empty) && color == invertColor(cp p) = const Broken
  | color == Empty = cp
  | otherwise = \v -> case v of
      _ | v == p -> color
        | otherwise -> cp v
visitVertice :: Graph -> Int -> Graph
visitVertice graph vertice lambdaVertice 
  | vertice == lambdaVertice = emptySet
  | otherwise = graph lambdaVertice
  
buildGraph :: [(Int, Int)] -> Graph
buildGraph = foldl addEdge emptyGraph

readMatrix :: Int -> Int -> [String] -> [(Int, Int)]
readMatrix remain row strings
  | remain == 0 = []
  | otherwise = do
    let string = head strings
    let result = map (\(_,b) -> (b, row))(filter (\(a, _) -> a == "1")(zip (words string) [(0::Int)..]))
    let resultNext = readMatrix (remain - 1) (row + 1) (tail strings)
    result ++ resultNext
mergeColorProviders :: ColorProvider -> ColorProvider -> ColorProvider
mergeColorProviders cp1 cp2 value
  | cp1 value == Empty = cp2 value
  | cp2 value == Empty = cp1 value
  | cp1 value == cp2 value = cp1 value
  | cp1 value /= cp2 value = Broken
extractColor :: Graph -> Int-> Int -> ColorProvider -> ColorProvider
extractColor graph from count cp = do
  let current_color = cp from
  let inverted = invertColor current_color
  let neighs = filter (\x -> cp x `notElem` [inverted, Broken]) (setToList (graph from) count)
  let newCp = foldl (\x y -> setColor x (y, inverted)) cp neighs
  let recCp = foldl (\x y -> extractColor graph y count x) newCp neighs
  recCp

minim :: [Int] -> Int
minim []       = 0
minim [x]      = x
minim (x:xs)   = min x (minim xs)
putResult :: [Int] -> [Int] -> IO()
putResult a b = do
  let x1 = foldl (\x y -> x++show y ++ " ") "" a
  let x2 = (show 0)
  let x3 = foldl (\x y -> x++show y ++ " ") "" b
  writeFile "out.txt" (x1++"\n"++x2++"\n"++x3)
main :: IO ()
main = do
  content <- readFile "in.txt"
  let mx = lines content
  let count = read(head mx)::Int
  let edges = readMatrix count 0 (tail mx)
  let graph = buildGraph edges
  let resultColorProvider = foldl (\x y -> if x y == Empty
                                           then extractColor graph y count (setColor x (y, Red))
                                           else x) emptyColorProvider [0..count-1]
  let colors = map (\x -> (x, resultColorProvider x)) [0..count-1]
  if Broken `elem` map snd colors
    then writeFile "out.txt" "N"
    else do
      let reds = map (\(x, _) -> x+1) (filter (\(_, y) -> y == Red) colors)
      let blues = map (\(x, _) -> x+1) (filter (\(_, y) -> y == Blue) colors)
      if minim reds < minim blues
        then putResult reds blues
        else putResult blues reds

