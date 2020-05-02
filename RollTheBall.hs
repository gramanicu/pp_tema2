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
    Printeaza nivelul current
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
addCell (t,pos) lvl = if validPosition pos corner then
        if (cellType cell) == emptySpace then Level (levelMatrix(lvl) // [(p, Cell t pos)| p <- [pos]]) else lvl
    else lvl
    where 
        cell = levelMatrix lvl! pos
        corner = (snd(Arr.bounds (levelMatrix lvl)))

{-
    Creeaza un nou nivel folosind o lista de celule si pozitia coltului din dreapta jos al tabelei. Se foloseste un foldl, folosind un nivel (care initial are doar
    casute goale) ca acumulator
-}
createLevel :: Position -> [(Char, Position)] -> Level
createLevel corner cells = foldl (\ lvl cell -> (addCell cell lvl)) (emptyLevel corner) cells 

{-
    Functie care incearca sa mute o celula intr-un spatiu liber.
    Daca casuta nu este libera sau incercam sa mutam intr-o pozitie din afara nivelului, 
    neschimbat.
-}
moveCell :: Position -> Directions -> Level -> Level
moveCell pos dir lvl = if validPosition tpos corner then
    if (cellType tcell) == emptySpace && not (sType `elem` (startCells ++ winningCells)) then
            Level (levelMatrix(setNew) // [(p, Cell emptySpace pos)| p <- [pos]])
        else lvl
    else lvl
    where 
        cell = levelMatrix lvl! pos
        tpos
            | dir == North = (x - 1, y)
            | dir == South = (x + 1, y)
            | dir == East = (x, y + 1)
            | otherwise = (x, y - 1)
        tcell = levelMatrix lvl! tpos
        corner = (snd(Arr.bounds (levelMatrix lvl)))
        setNew = Level (levelMatrix(lvl) // [(p, Cell (cellType cell) p) | p <- [tpos]])
        x = (fst pos)
        y = (snd pos)
        sType = (cellType cell)

{-
    Verifica daca doua celule ADIACENTE sunt conectate
-}
connection :: Cell -> Cell -> Bool
connection scell tcell
    | stype `elem` connR && ttype `elem` connL && dir == West = True
    | stype `elem` connL && ttype `elem` connR && dir == East = True
    | stype == horPipe && ttype == horPipe && dir `elem` horDir = True
    | stype `elem` connD && ttype `elem` connU && dir == South = True
    | stype `elem` connU && ttype `elem` connD && dir == North = True
    | stype == verPipe && ttype == verPipe && dir `elem` verDir = True
    | otherwise = False
    where 
        connR = [horPipe, topLeft, botLeft, startRight, winRight]
        connL = [horPipe, topRight, botRight, startLeft, winLeft]
        connU = [verPipe, botRight, botLeft, startUp, winUp]
        connD = [verPipe, topRight, topLeft, startDown, winDown]
        horDir = [West, East]
        verDir = [North, South]
        stype = (cellType scell)
        ttype = (cellType tcell)
        dir = getDir scell tcell

{-
    Returneaza celula start dintr-o lista de celule
-}
getStartCell :: [Cell] -> Cell
getStartCell cells 
    | htype `elem` startCells = head cells
    | otherwise = getStartCell (tail cells)
    where
        htype = (cellType (head cells))

{-
    Intoarce casuta aflata la pozitia specificata
-}
getCell :: Level -> Position -> Cell
getCell lvl pos = levelMatrix lvl! pos

{-
    Intoarce la ce directie sa afla cea de-a doua celula fata de prima.
    Acestea TREBUIE sa fie ADIACENTE
-}
getDir :: Cell -> Cell -> Directions
getDir scell tcell
    | sx == tx + 1 && sy == ty = North
    | sx == tx - 1 && sy == ty = South
    | sx == tx && sy == ty + 1 = East
    | otherwise = West
    where
        spos = (cellPos scell)
        tpos = (cellPos tcell)
        sx = (fst spos)
        sy = (snd spos)
        tx = (fst tpos)
        ty = (snd tpos)

{-
    Intoarce o lista cu casutele adiacente nevizitate
-}
getNeighbours :: Level -> Position -> [Cell] -> [Cell]
getNeighbours lvl pos visited = 
    filter (\cell -> not (cell `elem` visited)) (map (\p -> getCell lvl p) dirV) 
    where
        x = (fst pos)
        y = (snd pos)
        corner = (snd(Arr.bounds (levelMatrix lvl)))
        dirU = (x - 1, y)
        dirD = (x + 1, y)
        dirL = (x , y - 1)
        dirR = (x , y + 1)
        dirV = filter (\ p -> validPosition p corner) [dirU, dirD, dirL, dirR]

{-
    Verifica daca este posibil sa ne aflam intr-o configuratie castigatoare. Acest lucru este posibil doar daca
    exista o conexiune la o celula adiacenta nevizitata.
-}
checkStillWinning :: Level -> Cell -> [Cell] -> Bool
checkStillWinning lvl cell visited
    | ctype `elem` winningCells = True
    | otherwise = or (map (\c -> checkStillWinning lvl c (visited ++ [c])) neighbours)
    where
        cpos = (cellPos cell)
        ctype = (cellType cell)
        neighbours = filter (\ c -> connection cell c) (getNeighbours lvl cpos (visited))

{-
    Verifica daca configuratia curenta a nivelului este castigatoare
-}
wonLevel :: Level -> Bool
wonLevel lvl = checkStillWinning lvl startc [startc]
    where
        startc = getStartCell (elems (levelMatrix lvl))

instance ProblemState Level (Position, Directions) where
    successors = undefined
    isGoal = undefined
    reverseAction = undefined
