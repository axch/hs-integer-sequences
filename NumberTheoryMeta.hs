{-# LANGUAGE TemplateHaskell #-}

module NumberTheoryMeta where

import Data.Maybe
import Data.Char
import Data.List.Split -- from cabal install split but apparently part of newer instances of Haskell Platform
import Language.Haskell.TH

transformer :: Name -> Q Exp
transformer name = do
  seqN <- newName "x"
  goodSeqN <- newName "p"
  viewN <- newName "f"
  -- Explicitly construct the pattern because I am matching a record
  -- with dynamic field labels.
  let pat = AsP goodSeqN (RecP (mkName "PartialSequence") -- TODO Bring this in as a Name by hacking module imports?
                               [(mkName from, ConP 'Data.Maybe.Just [VarP viewN]),
                                (mkName to, ConP 'Data.Maybe.Nothing [])])
  body <- [| Just ($(recUpdE (varE goodSeqN) [fieldExp (mkName to) [| Just ($(varE name) $(varE viewN)) |]]) ) |]
  -- Explicitly construct the lambda because I explicitly constructed
  -- its pattern.
  return $ LamE [VarP seqN] (CaseE (VarE seqN) [ Match pat (NormalB body) []
                                               , Match WildP (NormalB $ ConE 'Nothing) []])
    where [from,(c:to')] = splitOn "To" $ nameBase name
          to = (toLower c:to')
          

