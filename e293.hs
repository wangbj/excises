-- https://www.reddit.com/r/dailyprogrammer/comments/5e4mde/20161121_challenge_293_easy_defusing_the_bomb/
import qualified Data.Map as Map
import           Data.Map (Map)

data Color = White | Red | Black | Orange | Green | Purple
  deriving (Show, Read, Enum, Eq, Ord)

getNexts :: Color -> [Color]
getNexts White  = [Red, Orange, Green, Purple]
getNexts Red    = [Green]
getNexts Black  = [Red, Black, Purple]
getNexts Orange = [Red, Black]
getNexts Green  = [Orange, White]
getNexts Purple = [Red, Black]

fromList :: [Color] -> Map Color Int
fromList = Map.fromListWith (+) . flip zip (repeat 1)

-- ^ cut a wire and update the existing wires.
defuseWire :: Color -> Map Color Int -> Maybe (Map Color Int)
defuseWire c m = Map.lookup c m >>= \v -> return $ Map.update (if v < 2 then const Nothing else Just . pred) c m

defuseFrom :: Color -> Maybe (Map Color Int) -> Bool
defuseFrom _ Nothing = False
defuseFrom c (Just m)
  | Map.null m = c `elem` [White, Black, Purple]
  | otherwise = or . map (\(c', m') -> defuseFrom c' (defuseWire c' m')) $ zip (getNexts c) (repeat m)

-- ^ defuse set of wires, not assuming cuts are ordered
-- The quiz it self assumes cuts are ordered, which is even simpler.
defuse :: [Color] -> Bool
defuse cs = or $ map (\c -> defuseFrom c (Just m)) t
  where m  = fromList cs
        t = enumFromTo White Purple

defuseE293 :: [Color] -> Bool
defuseE293 [] = True
defuseE293 (c:[]) = c `elem` [White, Black, Purple]
defuseE293 (c:c':cs)
  | c' `elem` (getNexts c) = defuseE293 (c':cs)
  | otherwise              = False

--
ex1 = [White, Red, Green, White]
ex2 = [White, Orange, Green, White]

