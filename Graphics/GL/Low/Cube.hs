{-# LANGUAGE Rank2Types #-}
module Graphics.GL.Low.Cube ( 
    Cube(..),
    Side
    ) where

import Data.Functor
import Data.Foldable
import Control.Applicative
import Data.Traversable
import Data.Monoid

import Graphics.GL.Low.Internal.Types


-- | A type to pick one of the sides of a cube. See the accessors of the
-- type 'Cube'.
type Side = forall a . Cube a -> a

