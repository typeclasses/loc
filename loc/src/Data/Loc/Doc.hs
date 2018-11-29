{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Data.Loc.Doc
  ( splitDocAt
  ) where

import Data.Loc.Loc
import Data.Loc.Pos

import Data.ListLike (ListLike)

import qualified Data.ListLike as LL

type Doc doc line char = (ListLike doc line, ListLike line char)

splitDocAt :: Doc d l c => Loc -> d -> (d, d)
splitDocAt x lines = (before, after)
  where
    before = LL.append top left
    after = LL.append right bottom
    (top, lines') = LL.genericSplitAt (toNat (line x)) lines
    (mid, bottom) =
        case LL.uncons lines' of
            Nothing -> (LL.empty,
            Just
