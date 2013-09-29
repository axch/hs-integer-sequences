{-# LANGUAGE TemplateHaskell #-}

module NumberTheoryMeta where

import Data.Maybe
import Data.Char
import Data.List.Split -- from cabal install split but apparently part of newer instances of Haskell Platform
import Language.Haskell.TH

transformer :: String -> Q Exp
transformer name = do
  var1 <- newName "p"
  var2 <- newName "f"
  body <- [| Just ($(recUpdE (varE var1) [fieldExp (mkName to) [| Just ($(dyn name) $(varE var2)) |]]) ) |]
  -- Use explicit construction of the pattern because I am matching a
  -- record with dynamic field labels.
  return $ LamE [AsP var1 (RecP (mkName "PartialSequence")
                                [(mkName from, ConP 'Data.Maybe.Just [VarP var2]),
                                 (mkName to, ConP 'Data.Maybe.Nothing [])])]
                body
    where [from,(c:to')] = splitOn "To" name
          to = (toLower c:to')
          

