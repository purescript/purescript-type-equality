module Type.Equality
  ( class TypeEquals
  , to
  , from
  , leibniz
  , leibnizOp
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
class TypeEquals :: forall k. k -> k -> Constraint
class TypeEquals a b | a -> b, b -> a where
  leibniz :: forall p. p a -> p b

instance refl :: TypeEquals a a where
  leibniz a = a

newtype Op :: forall k. k -> k -> Type
newtype Op a b = Op (forall p. p b -> p a)

leibnizOp :: forall p a b. TypeEquals a b => p b -> p a
leibnizOp = case leibniz (Op (\pa -> pa) :: Op a a) of
  (Op f :: Op a b) -> f

newtype To a = To a

to :: forall a b. TypeEquals a b => a -> b
to a = case leibniz (To a) of
  To b -> b

from :: forall a b. TypeEquals a b => b -> a
from b = case leibnizOp (To b) of
  To a -> a
