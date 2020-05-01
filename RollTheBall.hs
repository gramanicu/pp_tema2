{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as Arr

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell 
    {
        cellType :: Char,
        cellPos :: Position
    } deriving (Eq, Ord)

instance Show Cell
    where show (Cell t _) = [t]

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level 
    {
        levelMatrix :: Arr.Array Position Cell
    }
    deriving (Eq, Ord)

{-

-}
instance Show Level 
    where show (Level arr) =  "\n"  ++ concat [ if snd ps /= snd (snd (Arr.bounds arr)) then show cell else show cell ++ "\n" | (ps, cell) <- assocs arr]

{-
    Creeaza un nou nivel. Parametrul pos specifica unde se afla coltul din dreapta jos al tablei. Tipul de data Level contine un Data.Array, definit intre (0,0) si pos.
    Apoi, valorile arrayului sunt setate cu ajutorul unui list comprehension.
-}
emptyLevel :: Position -> Level
emptyLevel pos = Level (array ((0,0), pos) [((x, y), (Cell emptySpace (x, y))) | x <- [0..fst (pos)], y <- [0..snd (pos)]])

{-
    Verifica daca o pozitie se afla pe table (x,y nu sunt mai mici ca 0 sau mai mari decat coltul din dreapta jos)
-}
validPosition :: Position -> Position -> Bool
validPosition pos corner = if x < 0 || y < 0 || x > fst corner || y > snd corner then False else True
    where 
        x = fst pos 
        y = snd pos

{-
    Adauga un cell de tipul precizat la o anumita pozitie. Prima oara se verifica daca pozitia este valida. Daca nu, nu se modifica nivelul. Daca casuta pe care
    o vom modifica este goala, vom pune noua casuta la acea pozitie. (Sintaxa e ciudata pentru operatia aceasta, dar merge)
-}
addCell :: (Char, Position) -> Level -> Level
addCell (t,pos) lvl = if validPosition pos (snd(Arr.bounds (levelMatrix lvl))) then
        if (cellType cell) == emptySpace then Level (levelMatrix(lvl) // [(p, Cell t pos)| p <- [pos]]) else lvl
    else lvl
    where cell = levelMatrix lvl! pos

{-
    Creeaza un nou nivel folosind o lista de celule si pozitia coltului din dreapta jos al tabelei. Se foloseste un foldl, folosind un nivel (care initial are doar
    casute goale) ca acumulator
-}
createLevel :: Position -> [(Char, Position)] -> Level
createLevel corner cells = foldl (\ lvl cell -> (addCell cell lvl)) (emptyLevel corner) cells 

{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell = undefined

{-
    *** TODO ***

    Verifică dacă două celule se pot conecta.
    Atenție: Această funcție se aplică de la stânga la 
    dreapta(nu este comutativă).

    ex: connection botLeft horPipe = True (╚═)
        connection horPipe botLeft = False (═╚)
-}
connection :: Cell -> Cell -> Bool
connection = undefined

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
wonLevel :: Level -> Bool
wonLevel = undefined

instance ProblemState Level (Position, Directions) where
    successors = undefined
    isGoal = undefined
    reverseAction = undefined
