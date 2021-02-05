module Data.Loc.SpanOrLoc
  (
    SpanOrLoc,

    -- * Constructing
    span, loc, fromTo,

    -- * Deconstructing
    spanOrLoc,

    -- * Querying
    start, end,
  )
  where

import Data.Loc.Internal.Prelude

import Data.Loc.Loc (Loc)
import Data.Loc.Span (Span)

import qualified Data.Loc.Loc as Loc
import qualified Data.Loc.Span as Span

import Data.Data (Data)

{- |

A 'SpanOrLoc' consists of a start location and an end location.
The end location must be greater than or equal to the start location;
in other words, backwards spans are not permitted.

If the start and end location are the same, then the value is a 'Loc'.
If they differ, then the value is a 'Span'.

-}
data SpanOrLoc = Span Span | Loc Loc
  deriving (Data, Eq, Ord)

instance Show SpanOrLoc
  where
    showsPrec i = \case
        Span x -> Span.spanShowsPrec i x
        Loc x -> Loc.locShowsPrec i x

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

start :: SpanOrLoc -> Loc
start = spanOrLoc Span.start id

end :: SpanOrLoc -> Loc
end = spanOrLoc Span.end id
