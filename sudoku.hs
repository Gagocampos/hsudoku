import Data.List.Split
import Data.Char
import Data.List
import Data.Function
import Control.Monad
import Control.Applicative

data Celula = Fixed Int | Possible [Int] deriving (Show, Eq)
	
type Linha  = [Celula]
	
type Bloco = [Linha]

mostrarGrade :: Bloco -> String
mostrarGrade = unlines . map (unwords . map mostrarCelula)
  where
    mostrarCelula (Fixed x) = show x
    mostrarCelula _ = "."
	
mostrarGradeComPossibilidades :: Bloco -> String
mostrarGradeComPossibilidades = unlines . map (unwords . map mostrarCelula)
  where
    mostrarCelula (Fixed x)     = show x ++ "          "
    mostrarCelula (Possible xs) =
      (++ "]")
      . Data.List.foldl' (\acc x -> acc ++ if x `elem` xs then show x else " ") "["
      $ [1..9]
	
podarCelulas :: [Celula] -> Maybe [Celula]
podarCelulas celulas = traverse podarCelula celulas
  where
    fixeds = [x | Fixed x <- celulas]
    podarCelula (Possible xs) = case xs Data.List.\\ fixeds of
      []  -> Nothing
      [y] -> Just $ Fixed y
      ys  -> Just $ Possible ys
    podarCelula x = Just x

blocoCheio :: Bloco -> Bool	
blocoCheio grade = null [ () | Possible _ <- concat grade ]
blocoInvalido :: Bloco -> Bool
blocoInvalido grade =
  any linhaInvalida grade
  || any linhaInvalida (Data.List.transpose grade)
  || any linhaInvalida (subBlocosToLinhas grade)
  where
    linhaInvalida linha =
      let fixeds         = [x | Fixed x <- linha]
          emptyPossibles = [x | Possible x <- linha, null x]
      in temDuplas fixeds || not (null emptyPossibles)
    temDuplas l = temDuplas' l []
    temDuplas' [] _ = False
    temDuplas' (y:ys) xs
      | y `elem` xs = True
      | otherwise   = temDuplas' ys (y:xs)

subBlocosToLinhas :: Bloco -> Bloco	
subBlocosToLinhas =
  concatMap (\linhas -> let [r1, r2, r3] = map (Data.List.Split.chunksOf 3) linhas
                      in zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3)
  . Data.List.Split.chunksOf 3

podarBloco' :: Bloco -> Maybe Bloco
podarBloco' grade =
  traverse podarCelulas grade
  >>= fmap Data.List.transpose . traverse podarCelulas . Data.List.transpose
  >>= fmap subBlocosToLinhas . traverse podarCelulas . subBlocosToLinhas

	
podarBloco :: Bloco -> Maybe Bloco
podarBloco = fixM podarBloco'
  where
    fixM f x = f x >>= \x' -> if x' == x then return x else fixM f x'

proximosBlocos :: Bloco -> (Bloco, Bloco)
proximosBlocos grade =
  let (i, first@(Fixed _), rest) =
        reparar
        . Data.List.minimumBy (compare `Data.Function.on` (possibilidades . snd))
        . filter (isPossible . snd)
        . zip [0..]
        . concat
        $ grade
  in (replace2D i first grade, replace2D i rest grade)
  where
    isPossible (Possible _) = True
    isPossible _            = False
    possibilidades (Possible xs) = length xs
    possibilidades (Fixed _)     = 1
    reparar (i, Possible [x, y]) = (i, Fixed x, Fixed y)
    reparar (i, Possible (x:xs)) = (i, Fixed x, Possible xs)
    reparar _                    = error "Caso impossivel!"
    replace2D :: Int -> a -> [[a]] -> [[a]]
    replace2D i v =
      let (x, y) = (i `quot` 9, i `mod` 9) in replace x (replace y (const v))
    replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0..]]

resolver :: Bloco -> Maybe Bloco
resolver grade = podarBloco grade >>= resolver'
  where
    resolver' g
      | blocoInvalido g = Nothing
      | blocoCheio g  = Just g
      | otherwise       =
          let (grade1, grade2) = proximosBlocos g
          in resolver grade1 <|> resolver grade2

lerGrade :: String -> Maybe Bloco
lerGrade s	
  | length s == 81 = traverse (traverse lerCelula) . Data.List.Split.chunksOf 9 $ s
  | otherwise      = Nothing
  where
    lerCelula '.' = Just $ Possible [1..9]
    lerCelula c
      | Data.Char.isDigit c && c > '0' = Just . Fixed . Data.Char.digitToInt $ c
      | otherwise = Nothing

main :: IO ()
main = do
    inputs <- lines <$> getContents
    Control.Monad.forM_ inputs $ \input ->
        case lerGrade input of
            Nothing   -> putStrLn "Entrada invalida!"
            Just grade -> case resolver grade of
                Nothing    -> putStrLn "Nenhuma solucao encontrada..."
                Just grade' -> putStrLn $ mostrarGrade grade'
