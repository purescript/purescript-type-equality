module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Newtype (class Newtype, unwrap)
import Type.Equality (class TypeEquals, proof, to, from)

newtype RecordNewtype = RecordNewtype
  { message :: String }

instance newtypeRecordNewtype ::
  TypeEquals inner { message :: String }
    => Newtype RecordNewtype inner where
  wrap = RecordNewtype <<< to
  unwrap (RecordNewtype rec) = from rec

message :: forall ty row. TypeEquals row ( message :: String ) => Newtype ty (Record row) => ty -> String
message = unwrap >>> proof >>> _.message

main :: Effect Unit
main = log (message (RecordNewtype { message: "Done" }))
