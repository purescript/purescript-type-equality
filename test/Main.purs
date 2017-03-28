module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Newtype (class Newtype, unwrap)
import Type.Equality (class TypeEquals, to, from)
import Type.Row.Effect.Equality as REE

newtype RecordNewtype = RecordNewtype
  { message :: String }

instance newtypeRecordNewtype ::
  TypeEquals inner { message :: String }
    => Newtype RecordNewtype inner where
  wrap = RecordNewtype <<< to
  unwrap (RecordNewtype rec) = from rec

class Foo f where
  foo :: String -> f Unit

instance fooEff :: REE.EffectRowEquals eff (console :: CONSOLE | e) => Foo (Eff eff) where
  foo = REE.effFrom <<< log

main :: Eff (console :: CONSOLE) Unit
main = foo (unwrap (RecordNewtype { message: "Done" })).message
