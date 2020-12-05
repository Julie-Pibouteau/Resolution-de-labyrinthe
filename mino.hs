--PARTIE 1 : ENTREE/SORTIE ET REPRESENTATION ABSTRAITE
import System.IO

data Tile = W | EU | EV | T | M deriving (Show,Eq)
type Game = ([[Tile]], (Int,Int))

--Le type Game est une représentation abstraite de l’état du jeu : il contient une description du labyrinthe et les coordonnées actuelles de Thésée
--On considère que le point en haut à gauche a pour coordonnées (0,0)

--QUESTION 1 :Une fonction readGame :: Handle -> IO Game qui prend comme argument le descriptif d’un fichier (qui a déjà été ouvert), lit et retourne la description du labyrinthe.

string2Tile :: String -> Tile
string2Tile "*" = W
string2Tile "o" = EU
string2Tile "x" = EV
string2Tile "T" = T
string2Tile "M" = M

tile2String :: Tile -> String
tile2String W = "*"
tile2String EU = "o"
tile2String EV = "x"
tile2String T = "T"
tile2String M = "M"

--à utiliser sur des chaînes déjà passées par words
makeLine :: [String] -> [Tile]
makeLine line = map string2Tile line

makeLab :: [String] -> Int -> [[Tile]]
makeLab [] width = []
makeLab lab width = (makeLine (take width lab)) : (makeLab (drop width lab) width)

--à utiliser sur des chaînes déjà passées par words
coordThesee :: [String] -> Int -> Int -> (Int,Int)
coordThesee (x:xs) n width
                            |x == "T"     = (mod n width, div n width)
                            |otherwise    = coordThesee xs (n+1) width

readGame :: Handle -> IO Game
readGame hInput = do
    nbLines <- hGetLine hInput
    let height = read nbLines :: Int
    nbColumns <- hGetLine hInput
    let width = read nbColumns :: Int
    labInput <- hGetContents hInput
    let game = (makeLab (words labInput) width,coordThesee (words labInput) 0 width)
    return game

 --QUESTION 2 : Une fonction showGame :: Game -> String qui retourne une chaı̂ne de caractères qui représente l’état actuel du jeu

showLine :: [Tile] -> String
showLine [] = ""
showLine (t:ts)
              |tile2String t == "x" = "\ESC[47mx \ESC[m" ++ (showLine ts)
              |otherwise          = (tile2String t) ++ " " ++ (showLine ts)

showGame :: Game -> String
showGame ([], _ ) = ""
showGame ((line:lines),(xThesee, yThesee)) = (showLine line) ++ "\n" ++ (showGame (lines,(xThesee, yThesee)))

--QUESTION 3 : Une fonction main qui fait l’interaction avec l’utilisateur (demande le nom du fichier, affiche le labyrinthe,etc.)

displaySolution :: Maybe Game -> String
displaySolution Nothing     = "Il n'existe pas de chemin permettant à Thésée de rejoindre le minotaure."
displaySolution (Just game)   = showGame game

main = do
    putStrLn "File where labyrinth is stored :"
    fileName <- getLine
    --ouvrir le fichier contenant le labyrinthe
    hInput <- openFile fileName ReadMode
    --lire le fichier
    game <- readGame hInput

    putStrLn "Labyrinth read :"
    --afficher le labyrinthe lu
    putStrLn (showGame game)

    putStrLn "Solution :"
    let solution = displaySolution (solve game)
    putStrLn solution
    hClose hInput

--PARTIE 2 : TROUVER LE MINOTAURE

{-
QUESTION 1 :step :: Game -> [Game], qui étant donné l’état actuel, produit une liste avec tous les états suivants possibles.Les règles ici sont
(i) Thésée peut faire un mouvement d’un point, vertical ou horizontal (mais pas diagonal)
(ii) Thésée ne peut pas se trouver sur un point de type W
(iii) tous les points déjà visités ont le type EV
(iv) Thésée ne se rend jamais une deuxième fois dans un point de type EV (on cherche le plus court chemin, donc ce n’est pas la peine de répéter de mouvements)
(v) si Thésée atteint le Minotaure le jeu est terminé.
-}

-- renvoie le nouveau jeu et si on a réussi à modifier à le modifier
goAbove :: Game -> (Game,Bool)
goAbove (lab, (xThesee, yThesee))
                                |(lab !! yThesee !! xThesee) == M    = ((lab, (xThesee, yThesee)),True) -- on a trouvé le minotaure
                                |yThesee <= 0   = ((lab, (xThesee, yThesee)),False)
                                |otherwise      = if (above == M) then ((lab, (xThesee, yThesee-1)),True)
                                                  else if (above == EU) then ((newLab, (xThesee, yThesee-1)), True)
                                                  else ((lab, (xThesee, yThesee)),False)
                                where
                                lineAbove = lab !! (yThesee -1)
                                lineThesee = lab !! yThesee
                                above = lineAbove !! xThesee
                                newLineAbove = (take xThesee lineAbove) ++ [EV] ++ (drop (xThesee +1) lineAbove)
                                newLab = (take (yThesee -1) lab) ++ [newLineAbove] ++ (drop yThesee lab)

goBelow :: Game -> (Game,Bool)
goBelow (lab, (xThesee, yThesee))
                                |(lab !! yThesee !! xThesee) == M    = ((lab, (xThesee, yThesee)),True) -- on a trouvé le minotaure
                                |yThesee >= ((length lab)-1)    = ((lab, (xThesee, yThesee)),False)
                                |otherwise      = if (below == M) then ((lab, (xThesee, yThesee+1)),True)
                                                  else if (below == EU) then ((newLab, (xThesee, yThesee+1)), True)
                                                  else ((lab, (xThesee, yThesee)),False)
                                where
                                lineBelow = lab !! (yThesee +1)
                                lineThesee = lab !! yThesee
                                below = lineBelow !! xThesee
                                newLineBelow = (take xThesee lineBelow) ++ [EV] ++ (drop (xThesee +1) lineBelow)
                                newLab = (take (yThesee+1) lab) ++ [newLineBelow] ++ (drop (yThesee +2) lab)


goLeft :: Game -> (Game,Bool)
goLeft (lab, (xThesee, yThesee))
                                |(lab !! yThesee !! xThesee) == M    = ((lab, (xThesee, yThesee)),True) -- on a trouvé le minotaure
                                |xThesee <= 0   = ((lab, (xThesee, yThesee)),False)
                                |otherwise      = if (left == M) then ((lab, (xThesee-1, yThesee)),True)
                                                  else if (left == EU) then ((newLab, (xThesee-1, yThesee)), True)
                                                  else ((lab, (xThesee, yThesee)),False)
                                where
                                lineThesee = lab !! yThesee
                                left = lineThesee !! (xThesee - 1)
                                newLineThesee = (take (xThesee-1) lineThesee) ++ [EV] ++ (drop xThesee lineThesee)
                                newLab = (take yThesee lab) ++ [newLineThesee] ++ (drop (yThesee +1) lab)


goRight :: Game -> (Game,Bool)
goRight (lab, (xThesee, yThesee))
                                |(lab !! yThesee !! xThesee) == M    = ((lab, (xThesee, yThesee)),True) -- on a trouvé le minotaure
                                |xThesee >= ((length (lab !! 0))-1)  = ((lab, (xThesee, yThesee)),False)
                                |otherwise      = if (right == M) then ((lab, (xThesee+1, yThesee)),True)
                                                  else if (right == EU) then ((newLab, (xThesee+1, yThesee)), True)
                                                  else ((lab, (xThesee, yThesee)),False)
                                where
                                lineThesee = lab !! yThesee
                                right = lineThesee !! (xThesee +1)
                                newLineThesee = (take (xThesee+1) lineThesee) ++ [EV] ++ (drop (xThesee+2) lineThesee)
                                newLab = (take yThesee lab) ++ [newLineThesee] ++ (drop (yThesee +1) lab)

--ne garde que les jeux qui ont avancé, mais aussi ceux où le minotaure a été trouvé
stepCheck :: [(Game,Bool)] -> [Game]
stepCheck [] = []
stepCheck ((g,b):gs)
                    |b          = g:(stepCheck gs)
                    |otherwise  = stepCheck gs

--si on a trouvé le minotaure, alors les coordonnées finales de Thésée sont celles du minotaure donc il suffit de tester si la tile correspondante vaut M.
step :: Game -> [Game]
step game = stepCheck [goAbove game, goBelow game, goLeft game, goRight game]


{-QUESTION 2 : Une fonction solveink :: Int -> Game -> [Game] qui prend comme paramètre un entier k
et l’état actuel et retourne une liste qui représente tous les chemins qui atteignent le Minotaure avec ≤ k
mouvements de Thésée. On vous conseille (mais ce n’est pas obligatoire de suivre ce conseil) de profiter du fait que les listes
sont des Monads. Par conséquent, c’est possible d’enchaı̂ner les applications de step en utilisant l’opérateur
>>=.-}

foundMino :: Game -> Bool
foundMino (lab,(xThesee,yThesee))
                                |(lab !! yThesee !! xThesee) == M = True
                                |otherwise                        = False


--séparer les jeux qui ont trouvé le minotaure des autres : renvoie la liste des jeux finis
finishedGames :: [Game] -> [Game]
finishedGames [] = []
finishedGames (g:gs)
                    |foundMino g    = g : (finishedGames gs)
                    |otherwise      = finishedGames gs

--séparer les jeux qui ont trouvé le minotaure des autres : renvoie la liste des jeux encore en cours
ongoingGames :: [Game] -> [Game]
ongoingGames [] = []
ongoingGames (g:gs)
                    |foundMino g    = ongoingGames gs
                    |otherwise      = g : (ongoingGames gs)

--donne la liste de tous les jeux qu'on peut obtenir en k déplacements exactement, avec éventuellement des chemins qui n'atteignent pas le minotaure
solveinkAux :: Int -> Game -> [Game]
solveinkAux 0 game = [game]
solveinkAux k game = ((ongoingGames res) >>= step) ++ (finishedGames res)
                          where res = solveinkAux (k-1) game

--supprime les chemins qui n'atteignent pas le minotaure
reachM :: [Game] -> [Game]
reachM [] = []
reachM (g:gs)
            |foundMino g   = g : (reachM gs)
            |otherwise     = reachM gs

solveink :: Int -> Game -> [Game]
solveink k game = reachM (solveinkAux k game)


{-QUESTION 3 : Une fonction solve :: Game -> Maybe Game qui prend comme paramètre l’état actuel et re-
tourne une solution qui représente le plus court chemin, ou Nothing si aucune solution n’existe. (Si
plusieurs solutions optimales existent, vous pouvez retourner n’importe laquelle parmi elles). -}

--le chemin le plus court est celui avec le moins de croix

lengthPath :: Game -> Int
lengthPath ([],_) = 0
lengthPath ((line:lines),(xThesee, yThesee)) = (length [tile | tile <- line, tile == EV]) + (lengthPath (lines, (xThesee, yThesee)))

shortestPath :: [Game] -> Maybe Game
shortestPath [] = Nothing
shortestPath [g] = Just g
shortestPath (g1:g2:gs)
                      |lengthPath g1 < lengthPath g2  = shortestPath (g1:gs)
                      |otherwise                      = shortestPath (g2:gs)

--au maximum, le chemin entre Thesee et le minotaure fait height*width du labyrinthe
solve :: Game -> Maybe Game
solve (lab,(xThesee, yThesee)) = shortestPath (solveink n (lab,(xThesee, yThesee)))
                                        where
                                        n = (length lab)*(length (lab !! 0))
