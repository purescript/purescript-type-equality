module Type.Row.Effect.Equality
  ( class EffectRowEquals
  , to
  , from
  , effTo
  , effFrom
  ) where

import Control.Monad.Eff (kind Effect)

-- | This type class asserts that effect rows `a` and `b` are equal.
-- |
-- | The functional dependencies and the single instance below will force the
-- | two type arguments to unify when either one is known.
-- |
-- | Note: any instance will necessarily ovelap with `refl` below, so instances
-- | of this class should not be defined in libraries.
class EffectRowEquals (a :: # Effect) (b :: # Effect) | a -> b, b -> a where
  to :: forall r. r a -> r b
  from :: forall r. r b -> r a

instance refl :: EffectRowEquals a a where
  to a = a
  from a = a

newtype Flipmode e a eff = Flipmode (e eff a)

unflip :: forall e a eff. Flipmode e a eff -> e eff a
unflip (Flipmode e) = e

-- | A version of `to` that can be applied to types like `Eff`, `Aff`, etc.
effTo :: forall e a b x. EffectRowEquals a b => e a x -> e b x
effTo e = unflip (to (Flipmode e))

-- | A version of `from` that can be applied to types like `Eff`, `Aff`, etc.
effFrom :: forall e a b x. EffectRowEquals a b => e b x -> e a x
effFrom e = unflip (from (Flipmode e))
