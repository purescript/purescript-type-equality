module Type.Equality
  ( class TypeEquals
  , typeEqualsProof
  , to
  , from
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
  typeEqualsProof :: forall p. p a -> p b

instance refl :: TypeEquals a a where
  typeEqualsProof a = a

newtype Op :: forall k. k -> k -> Type
newtype Op a b = Op (forall p. p b -> p a)

symm :: forall a b. (forall p. p a -> p b) -> (forall p. p b -> p a)
symm proof = case proof (Op (\pa -> pa) :: Op a a) of
  (Op f :: Op a b) -> f

to :: forall a b. TypeEquals a b => a -> b
to = typeEqualsProof \a -> a

from :: forall a b. TypeEquals a b => b -> a
from = symm typeEqualsProof \b -> b
