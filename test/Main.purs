module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Newtype (class Newtype, unwrap)
import Type.Equality (class TypeEquals, to, from)

newtype RecordNewtype = RecordNewtype
  { message :: String }

instance newtypeRecordNewtype ::
  TypeEquals inner { message :: String }
    => Newtype RecordNewtype inner where
  wrap = RecordNewtype <<< to
  unwrap (RecordNewtype rec) = from rec

main :: Eff (console :: CONSOLE) Unit
main = log (unwrap (RecordNewtype { message: "Done" })).message
