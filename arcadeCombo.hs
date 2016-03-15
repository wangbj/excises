{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict(HashMap)
import Control.Monad.State
import Data.List

type KeyStroke = Char
type KS = [KeyStroke]

cacheSize = 100

type MyState = (KS, Int, HashMap KS String)

combo1 = ("UDLR", "combo UDLR")
combo2 = ("LR", "back and forth")

hash1 = HashMap.fromList [combo1, combo2]
st1 = ("", cacheSize, hash1) :: MyState

register :: (KS, String) -> StateT MyState IO  ()
register (combo, desc) = do
  (ks, longest, hash) <- get
  let hash' = HashMap.insert combo desc hash
      !len = length combo
  put (ks, max longest len, hash')

match ks hash = concatMap go assocs
  where assocs = zip (HashMap.keys hash) (HashMap.elems hash)
        go (k, v) = if k `isSuffixOf` ks then [v] else []

keypress c = do
  (ks, longest, hash) <- get
  let ks' = take longest (c:ks)
  put (ks', longest, hash)
  let ksRev = reverse ks'
  if null ksRev then return [] else do
    return $! match ksRev hash

stroke press = do
  res <- press
  when ( (not . null) res ) (mapM_ (liftIO . putStrLn) res)

ex1 = do
  register combo1
  register combo2
  stroke $ keypress 'U'
  stroke $ keypress 'U'
  stroke $ keypress 'D'
  stroke $ keypress 'L'
  stroke $ keypress 'R'
  stroke $ keypress 'L'
  register ("UUDLRLR", "longest combo")
  stroke $ keypress 'R'

main = evalStateT ex1 st1
