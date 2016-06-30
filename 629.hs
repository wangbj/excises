data Dir = N | W | S | E deriving (Eq, Show)
data Pos = Pos Dir Int Int deriving Show
data Cmd = L | R | A deriving (Eq, Read, Show)

moveLeft (Pos N x y) = Pos W (x-1) y
moveLeft (Pos W x y) = Pos S x (y-1)
moveLeft (Pos S x y) = Pos E (x+1) y
moveLeft (Pos E x y) = Pos N x (y+1)

moveRight (Pos N x y) = Pos E (x+1) y
moveRight (Pos E x y) = Pos S x (y-1)
moveRight (Pos S x y) = Pos W (x-1) y
moveRight (Pos W x y) = Pos N x (y+1)

moveAdvance (Pos N x y) = Pos N x (y+1)
moveAdvance (Pos W x y) = Pos W (x-1) y
moveAdvance (Pos S x y) = Pos S x (y-1)
moveAdvance (Pos E x y) = Pos E (x+1) y

move :: Pos -> Cmd -> Pos
move p c
  | c == L = moveLeft p
  | c == R = moveRight p
  | c == A = moveAdvance p

maybeRead = flip lookup [ ('L', L), ('R', R), ('A', A)]

getMovements :: String -> Maybe [Cmd]
getMovements = mapM maybeRead

moveRobot :: Pos -> String -> Maybe Pos
moveRobot p = fmap (foldl move p) . getMovements
