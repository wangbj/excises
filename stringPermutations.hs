import Data.List

-- split list into group such as:
-- "abc" -> [ ('a', "bc"), ('b', "ac"), ('c', "ab") ]
select = go []
  where go l (x:xs) = (x, l++xs) : go (l++[x]) xs
        go l [] = []

-- permutations
-- called sort here because we want do string permutations with lexical order
-- this would out perform (sort . Data.List.permutations) 
-- Since we don't have to sort the permutation result (which could be quite large)
perm [] = [[]]
perm s = concatMap (\(x, xs) -> fmap (x:) (perm xs)) . select . sort $ s
