module Type.Equality
  ( class TypeEquals
  , to
  , from
  , class RowEquals
  , toR
  , fromR
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
class TypeEquals a b | a -> b, b -> a where
  to :: a -> b
  from :: b -> a

instance refl :: TypeEquals a a where
  to a = a
  from a = a

-- | This type class asserts that rows `a` and `b`
-- | are equal.
class RowEquals (a ∷ # *) (b ∷ # *) | a → b, b → a where
  toR ∷ ∀ r. r a → r b
  fromR ∷ ∀ r. r b → r a

instance reflR ∷ RowEquals r r where
  toR r = r
  fromR r = r
