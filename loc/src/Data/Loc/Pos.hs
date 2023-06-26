module Data.Loc.Pos
  ( Line,
    Column,
  )
where

import Data.Loc.Internal.Prelude
import Integer (Positive)
import Prelude (Num (..))

--------------------------------------------------------------------------------
--  Line
--------------------------------------------------------------------------------

newtype Line = Line Positive
  deriving newtype (Eq, Ord, Num, Integral, Real, Enum, Show)

--------------------------------------------------------------------------------
--  Column
--------------------------------------------------------------------------------

newtype Column = Column Positive
  deriving newtype (Eq, Ord, Num, Integral, Real, Enum, Show)
