module Gen where

import Data.List qualified as List
import Data.Loc qualified as Loc
import Data.Loc.Internal.Prelude
import Data.Loc.Types
import Data.Set qualified as Set
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prelude (Num (..))

--------------------------------------------------------------------------------
--  Parameter defaults
--------------------------------------------------------------------------------

-- | The default maximum line: 99.
defMaxLine :: Line
defMaxLine = 99

-- | The default maximum column number: 99.
defMaxColumn :: Column
defMaxColumn = 99

--------------------------------------------------------------------------------
--  Bounds
--------------------------------------------------------------------------------

-- | Inclusive lower and upper bounds on a range.
type Bounds a = (a, a)

-- | The size of a range specified by 'Bounds'
--
-- Assumes the upper bound is at least the lower bound.
boundsSize :: Num n => (n, n) -> n
boundsSize (a, b) = 1 + b - a

--------------------------------------------------------------------------------
--  Pos
--------------------------------------------------------------------------------

-- | @'line' a b@ generates a line number on the linear range /a/ to /b/
line ::
  -- | Minimum and maximum line number
  Bounds Line ->
  Gen Line
line (a, b) = Gen.integral (Range.linear a b)

-- | Generates a line number within the default bounds @(1, 'defMaxLine')@
line' :: Gen Line
line' = line (1, defMaxLine)

-- | @'column' a b@ generates a column number on the linear range /a/ to /b/
column ::
  -- | Minimum and maximum column number
  Bounds Column ->
  Gen Column
column (a, b) = Gen.integral (Range.linear a b)

-- | Generates a column number within the default bounds @(1, 'defMaxColumn')@
column' :: Gen Column
column' = column (1, defMaxColumn)

--------------------------------------------------------------------------------
--  Loc
--------------------------------------------------------------------------------

-- | @'loc' lineBounds columnBounds@ generates a 'Loc' with the line number
-- bounded by @lineBounds@ and column number bounded by @columnBounds@
loc ::
  -- | Minimum and maximum line number
  Bounds Line ->
  -- | Minimum and maximum column number
  Bounds Column ->
  Gen Loc
loc lineBounds columnBounds =
  Loc.loc <$> line lineBounds <*> column columnBounds

-- | Generates a 'Loc' within the default line and column bounds
loc' :: Gen Loc
loc' = loc (1, defMaxLine) (1, defMaxColumn)

--------------------------------------------------------------------------------
--  Span
--------------------------------------------------------------------------------

-- | @'span' lineBounds columnBounds@ generates a 'Span' with start and end
-- positions whose line numbers are bounded by @lineBounds@ and whose column
-- numbers are bounded by @columnBounds@
span ::
  -- | Minimum and maximum line number
  Bounds Line ->
  -- | Minimum and maximum column number
  Bounds Column ->
  Gen Span
span lineBounds columnBounds@(minColumn, maxColumn) =
  let lines :: Gen (Line, Line)
      lines =
        line lineBounds >>= \a ->
          line lineBounds <&> \b ->
            (min a b, max a b)

      columnsDifferentLine :: Gen (Column, Column)
      columnsDifferentLine =
        column columnBounds >>= \a ->
          column columnBounds <&> \b ->
            (a, b)

      columnsSameLine :: Gen (Column, Column)
      columnsSameLine =
        column (minColumn + 1, maxColumn) >>= \a ->
          column columnBounds <&> \b ->
            case compare a b of
              EQ -> (a - 1, b)
              LT -> (a, b)
              GT -> (b, a)
   in lines >>= \(startLine, endLine) ->
        ( if startLine /= endLine
            then columnsDifferentLine
            else columnsSameLine
        )
          <&> \(startColumn, endColumn) ->
            let start = Loc.loc startLine startColumn
                end = Loc.loc endLine endColumn
             in Loc.spanFromTo start end

-- | Generates a 'Span' with start and end positions within the default line and
-- column bounds
span' :: Gen Span
span' = span (1, defMaxLine) (1, defMaxColumn)

--------------------------------------------------------------------------------
--  Area
--------------------------------------------------------------------------------

-- | @'area' lineBounds columnBounds@ generates an 'Area' consisting of 'Span's
-- with start and end positions whose line numbers are bounded by @lineBounds@
-- and whose column numbers are bounded by @columnBounds@
area ::
  -- | Minimum and maximum line number
  Bounds Line ->
  -- | Minimum and maximum column number
  Bounds Column ->
  Gen Area
area lineBounds columnBounds =
  fold . snd . mapAccumL f Nothing . Set.toAscList . Set.fromList <$> locs
  where
    gridSize :: Int =
      max
        (fromIntegral (boundsSize lineBounds))
        (fromIntegral (boundsSize columnBounds))

    locs :: Gen [Loc] =
      loc lineBounds columnBounds
        & List.repeat
        & List.take (gridSize `div` 5)
        & sequenceA

    f :: Maybe Loc -> Loc -> (Maybe Loc, Area)
    f prevLocMay newLoc =
      case prevLocMay of
        Just prevLoc -> (Nothing, Loc.areaFromTo prevLoc newLoc)
        Nothing -> (Just newLoc, mempty)

-- | Generates an 'Area' consisting of 'Span's with start and end positions within
-- the default line and column bounds
area' :: Gen Area
area' = area (1, defMaxLine) (1, defMaxColumn)
