import Data.List.Split
import Data.Char
import Data.List
import Data.Function
import Control.Monad
import Control.Applicative

--Definição dos elemento básicos do tabuleiro
data Celula = Fixed Int | Possible [Int] deriving (Show, Eq)

type Linha  = [Celula]
	
type Bloco = [Linha]

--Mostra o tabuleiro inteiro
mostrarGrade :: Bloco -> String
mostrarGrade = unlines . map (unwords . map mostrarCelula)
  where
    mostrarCelula (Fixed x) = show x
    mostrarCelula _ = "*"
	
--"Poda" o tabuleiro, eliminando os números já presentes
--em nas células fixas dos vetores de possibilidades das
--demais posições do tabuleiro
podarCelulas :: [Celula] -> Maybe [Celula]
podarCelulas celulas = traverse podarCelula celulas
  where
    fixeds = [x | Fixed x <- celulas]
    podarCelula (Possible xs) = case xs Data.List.\\ fixeds of
      []  -> Nothing
      [y] -> Just $ Fixed y
      ys  -> Just $ Possible ys
    podarCelula x = Just x

--Checa se o bloco está cheio
blocoCheio :: Bloco -> Bool	
blocoCheio grade = null [ () | Possible _ <- concat grade ]

--Checa se existe algo incorreto no bloco
blocoInvalido :: Bloco -> Bool
blocoInvalido grade =
  any linhaInvalida grade
  || any linhaInvalida (Data.List.transpose grade)
  || any linhaInvalida (subBlocosToLinhas grade)
  where
    linhaInvalida linha =
      let fixeds         = [x | Fixed x <- linha]
          emptyPossibles = [x | Possible x <- linha, null x]
      in temDuplicata fixeds || not (null emptyPossibles)
    temDuplicata l = temDuplicata' l []
    temDuplicata' [] _ = False
    temDuplicata' (y:ys) xs
      | y `elem` xs = True
      | otherwise   = temDuplicata' ys (y:xs)

--Transforma os sub-blocos 2x3 em uma linha inteira
--para possibilitar a "podagem" do tabuleiro
subBlocosToLinhas :: Bloco -> Bloco	
subBlocosToLinhas =
  concatMap (\linhas -> let [r1, r2, r3] = map (Data.List.Split.chunksOf 2) linhas
                      in zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3)
  . Data.List.Split.chunksOf 3

--Função auxiliar de podagem que é chamada toda vez que a
--condição de parada não é satisfeita na função principal
podarBloco' :: Bloco -> Maybe Bloco
podarBloco' grade =
  traverse podarCelulas grade
  >>= fmap Data.List.transpose . traverse podarCelulas . Data.List.transpose
  >>= fmap subBlocosToLinhas . traverse podarCelulas . subBlocosToLinhas

--Função principal de podagem, onde se encontram o loop
--e a condição de parada.
podarBloco :: Bloco -> Maybe Bloco
podarBloco = fixM podarBloco'
  where
    fixM f x = f x >>= \x' -> if x' == x then return x else fixM f x'


--Faz a escolha da próxima célula que será tratada
--como fixa com base no número de possibilidades que 
--existe em seu vetor e depois faz a substituição na
--nova grade.
proximosBlocos :: Bloco -> (Bloco, Bloco)
proximosBlocos grade =
  let (i, first@(Fixed _), rest) =
        celulaFixa
        . Data.List.minimumBy (compare `Data.Function.on` (numPossibilidades . snd))
        . filter (isPossible . snd)
        . zip [0..]
        . concat
        $ grade
  in (replace2D i first grade, replace2D i rest grade)
  where
    isPossible (Possible _) = True
    isPossible _            = False

    numPossibilidades (Possible xs) = length xs
    numPossibilidades (Fixed _)     = 1

    celulaFixa (i, Possible [x, y]) = (i, Fixed x, Fixed y)
    celulaFixa (i, Possible (x:xs)) = (i, Fixed x, Possible xs)
    celulaFixa _                    = error "Caso impossivel!"
    
    replace2D :: Int -> a -> [[a]] -> [[a]]
    replace2D i v =
      let (x, y) = (i `quot` 6, i `mod` 6) in replace x (replace y (const v))
    replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0..]]

--Função principal do programa que inicialmente poda
--o bloco, verifica se o mesmo é válido ou está cheio,
--e depois tenta resolver as duas próximas grades aplicando
--backtracking.
resolver :: Bloco -> Maybe Bloco
resolver grade = podarBloco grade >>= resolver'
  where
    resolver' g
      | blocoInvalido g = Nothing
      | blocoCheio g  = Just g
      | otherwise       =
          let (grade1, grade2) = proximosBlocos g
          in resolver grade1 <|> resolver grade2

--Lê a entrada e mapeia os valores para formar o 
--tabuleiro 6x6
lerGrade :: String -> Maybe Bloco
lerGrade s	
  | length s == 36 = traverse (traverse lerCelula) . Data.List.Split.chunksOf 6 $ s
  | otherwise      = Nothing
  where
    lerCelula '*' = Just $ Possible [1..6]
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
