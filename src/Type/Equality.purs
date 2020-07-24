module Type.Equality
  ( class TypeEquals
  , to
  , from
  , leibniz
  ) where

-- | This type class asserts that types `a` and `b`
-- | are equal.
-- |
-- | The functional dependencies and the single
-- | instance below will force the two type arguments
-- | to unify when either one is known.
-- |
-- | Note: any instance will necessarily overlap with
-- | `refl` below, so instances of this class should
-- | not be defined in libraries.
-- class TypeEquals :: forall k. k -> k -> Constraint
class TypeEquals a b | a -> b, b -> a where
  leibniz :: forall p. p a -> p b

instance refl :: TypeEquals a a where
  leibniz a = a

newtype To a = To a

to :: forall a b. TypeEquals a b => a -> b
to a = case leibniz (To a) of
  To b -> b

newtype From a b = From (b -> a)

from :: forall a b. TypeEquals a b => b -> a
from b = case leibniz (From (\a -> a) :: From a a) of
  (From f :: From a b) -> f b
