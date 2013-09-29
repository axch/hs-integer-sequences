{-# LANGUAGE TemplateHaskell #-}

module NumberTheoryMeta where

import Data.Maybe
import Data.Char
import Data.List.Split -- from cabal install split but apparently part of newer instances of Haskell Platform
import Language.Haskell.TH

transformer :: String -> Q Exp
transformer name = do
  var <- newName "x"
  var1 <- newName "p"
  var2 <- newName "f"
  -- Explicitly construct the pattern because I am matching a record
  -- with dynamic field labels.
  let pat = AsP var1 (RecP (mkName "PartialSequence")
                           [(mkName from, ConP 'Data.Maybe.Just [VarP var2]),
                            (mkName to, ConP 'Data.Maybe.Nothing [])])
  body <- [| Just ($(recUpdE (varE var1) [fieldExp (mkName to) [| Just ($(dyn name) $(varE var2)) |]]) ) |]
  -- Explicitly construct the lambda because I explicitly constructed
  -- its pattern.
  return $ LamE [VarP var] (CaseE (VarE var) [Match pat (NormalB body) [], Match WildP (NormalB $ ConE 'Nothing) []])
    where [from,(c:to')] = splitOn "To" name
          to = (toLower c:to')
          

