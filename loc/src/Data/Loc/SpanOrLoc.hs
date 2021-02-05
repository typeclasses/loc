{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Data.Loc.SpanOrLoc
  ( SpanOrLoc

  -- * Constructing
  , span
  , loc
  , fromTo

  -- * Deconstructing
  , spanOrLoc

  ) where

import Data.Loc.Internal.Prelude

import Data.Loc.Exception (LocException (..))
import Data.Loc.List.OneToTwo (OneToTwo)
import Data.Loc.List.ZeroToTwo (ZeroToTwo)
import Data.Loc.Loc (Loc, locReadPrec, locShowsPrec)
import Data.Loc.Span (Span)
import Data.Loc.Pos (Line)

import qualified Data.Loc.List.OneToTwo as OneToTwo
import qualified Data.Loc.List.ZeroToTwo as ZeroToTwo
import qualified Data.Loc.Loc as Loc
import qualified Data.Loc.Span as Span

import           Data.Data (Data)
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty

{- |

A 'SpanOrLoc' consists of a start location and an end location.
The end location must be greater than or equal to the start location;
in other words, backwards spans are not permitted.

If the start and end location are the same, then the value is a 'Loc'.
If they differ, then the value is a 'Span'.

-}
data SpanOrLoc = Span Span | Loc Loc
  deriving (Data, Eq, Ord)

span :: Span -> SpanOrLoc
span = Span

loc :: Loc -> SpanOrLoc
loc = Loc

spanOrLoc :: (Span -> a) -> (Loc -> a) -> SpanOrLoc -> a
spanOrLoc f _ (Span x) = f x
spanOrLoc _ f (Loc x) = f x

{- |

Construct a 'SpanOrLoc' from two 'Loc's. If the two locs are not equal,
the lesser loc will be the start, and the greater loc will be the end.

-}
fromTo :: Loc -> Loc -> SpanOrLoc
fromTo a b =
  maybe (Loc a) Span (Span.fromToMay a b)
