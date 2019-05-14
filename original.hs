import Data.List.Split
import Data.Char
import Data.List
import Data.Function
import Control.Monad
import Control.Applicative

data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)
	
type linha  = [Cell]
	
type Grid = [linha]

readGrid :: String -> Maybe Grid
readGrid s	
  | length s == 81 = traverse (traverse readCell) . Data.List.Split.chunksOf 9 $ s
  | otherwise      = Nothing
  where
    readCell '.' = Just $ Possible [1..9]
    readCell c
      | Data.Char.isDigit c && c > '0' = Just . Fixed . Data.Char.digitToInt $ c
      | otherwise = Nothing

showGrid :: Grid -> String
showGrid = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x) = show x
    showCell _ = "."
	
showGridWithPossibilities :: Grid -> String
showGridWithPossibilities = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x)     = show x ++ "          "
    showCell (Possible xs) =
      (++ "]")
      . Data.List.foldl' (\acc x -> acc ++ if x `elem` xs then show x else " ") "["
      $ [1..9]
	
pruneCells :: [Cell] -> Maybe [Cell]
pruneCells cells = traverse pruneCell cells
  where
    fixeds = [x | Fixed x <- cells]
    pruneCell (Possible xs) = case xs Data.List.\\ fixeds of
      []  -> Nothing
      [y] -> Just $ Fixed y
      ys  -> Just $ Possible ys
    pruneCell x = Just x

subGridsTolinhas :: Grid -> Grid	
subGridsTolinhas =
  concatMap (\linhas -> let [r1, r2, r3] = map (Data.List.Split.chunksOf 3) linhas
                      in zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3)
  . Data.List.Split.chunksOf 3

pruneGrid' :: Grid -> Maybe Grid
pruneGrid' grid =
  traverse pruneCells grid
  >>= fmap Data.List.transpose . traverse pruneCells . Data.List.transpose
  >>= fmap subGridsTolinhas . traverse pruneCells . subGridsTolinhas

	
pruneGrid :: Grid -> Maybe Grid
pruneGrid = fixM pruneGrid'
  where
    fixM f x = f x >>= \x' -> if x' == x then return x else fixM f x'

nextGrids :: Grid -> (Grid, Grid)
nextGrids grid =
  let (i, first@(Fixed _), rest) =
        repararCelula
        . Data.List.minimumBy (compare `Data.Function.on` (possibilityCount . snd))
        . filter (isPossible . snd)
        . zip [0..]
        . concat
        $ grid
  in (replace2D i first grid, replace2D i rest grid)
  where
    isPossible (Possible _) = True
    isPossible _            = False
    possibilityCount (Possible xs) = length xs
    possibilityCount (Fixed _)     = 1
    repararCelula (i, Possible [x, y]) = (i, Fixed x, Fixed y)
    repararCelula (i, Possible (x:xs)) = (i, Fixed x, Possible xs)
    repararCelula _                    = error "Impossible case"
    replace2D :: Int -> a -> [[a]] -> [[a]]
    replace2D i v =
      let (x, y) = (i `quot` 9, i `mod` 9) in replace x (replace y (const v))
    replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0..]]

isGridFilled :: Grid -> Bool	
isGridFilled grid = null [ () | Possible _ <- concat grid ]
isGridInvalid :: Grid -> Bool
isGridInvalid grid =
  any isInvalidlinha grid
  || any isInvalidlinha (Data.List.transpose grid)
  || any isInvalidlinha (subGridsTolinhas grid)
  where
    isInvalidlinha linha =
      let fixeds         = [x | Fixed x <- linha]
          emptyPossibles = [x | Possible x <- linha, null x]
      in hasDups fixeds || not (null emptyPossibles)
    hasDups l = hasDups' l []
    hasDups' [] _ = False
    hasDups' (y:ys) xs
      | y `elem` xs = True
      | otherwise   = hasDups' ys (y:xs)

solve :: Grid -> Maybe Grid
solve grid = pruneGrid grid >>= solve'
  where
    solve' g
      | isGridInvalid g = Nothing
      | isGridFilled g  = Just g
      | otherwise       =
          let (grid1, grid2) = nextGrids g
          in solve grid1 <|> solve grid2

main :: IO ()
main = do
    inputs <- lines <$> getContents
    Control.Monad.forM_ inputs $ \input ->
        case readGrid input of
            Nothing   -> putStrLn "Invalid input"
            Just grid -> case solve grid of
                Nothing    -> putStrLn "No solution found"
                Just grid' -> putStrLn $ showGrid grid'