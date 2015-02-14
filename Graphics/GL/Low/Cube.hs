{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Graphics.GL.Low.Cube where

import Data.Functor
import Data.Foldable
import Control.Applicative
import Data.Traversable
import Data.Monoid

-- | Six values, one on each side.
data Cube a = Cube
  { cubeRight  :: a
  , cubeLeft   :: a
  , cubeTop    :: a
  , cubeBottom :: a
  , cubeFront  :: a
  , cubeBack   :: a }
    deriving (Show, Functor, Foldable, Traversable)

-- | A type to pick one of the sides of a cube. See the accessors of the
-- type 'Cube'.
type Side = forall a . Cube a -> a

instance Applicative Cube where
  pure x = Cube x x x x x x
  (Cube f1 f2 f3 f4 f5 f6) <*> (Cube x1 x2 x3 x4 x5 x6) =
    Cube (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5) (f6 x6)

instance Monoid a => Monoid (Cube a) where
  mempty = Cube mempty mempty mempty mempty mempty mempty
  (Cube x1 x2 x3 x4 x5 x6) `mappend` (Cube y1 y2 y3 y4 y5 y6) = Cube
    (x1 <> y1)
    (x2 <> y2)
    (x3 <> y3)
    (x4 <> y4)
    (x5 <> y5)
    (x6 <> y6)
  
