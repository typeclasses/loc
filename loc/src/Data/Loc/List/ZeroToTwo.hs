module Data.Loc.List.ZeroToTwo
  ( -- Imports
    -- $imports

    -- * Type
    ZeroToTwo (..),
  )
where

import Data.Loc.Internal.Prelude

-- | List of length 0, 1, or 2
data ZeroToTwo a
  = -- | List of length 0
    Zero
  | -- | List of length 1
    One a
  | -- | List of length 2
    Two a a
  deriving (Eq, Ord, Show, Read, Foldable, Functor)

-- $imports
--
-- Recommended import:
--
-- > import Data.Loc.List.ZeroToTwo (ZeroToTwo)
-- > import qualified Data.Loc.List.ZeroToTwo as ZeroToTwo
