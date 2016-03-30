evalPrefix s = acc [] Nothing (C.words s)
  where acc _ opr []      = opr
        acc op opr (x:xs) = case C.readInt x of
          Nothing     -> lookup (C.head x) table >>= \f -> acc (f:op) opr xs
          Just (y, _) -> case opr of
            Nothing   -> acc op (Just y) xs
            Just z    -> acc (tail op) (Just ((head op) z y)) xs
        table = [   ('+', (+)) , ('-', (-)) , ('*', (*)) , ('/', div) ]
