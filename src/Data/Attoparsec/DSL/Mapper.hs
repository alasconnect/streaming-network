{-# LANGUAGE LambdaCase #-}

module Data.Attoparsec.DSL.Mapper where

  import Data.ByteString (ByteString)
  import qualified Data.HashMap as HM
  import Data.Scientific (Scientific)

  data Value
    = Object !Object
    | Array ![Value]
    | String !ByteString
    | Number !Scientific
    | Bool !Bool
    
  type Object = HM.Map ByteString Value

  empty :: Object
  empty = HM.empty :: HM.Map ByteString Value

  upsert :: ByteString -> Value -> Object -> Object
  upsert k a o = HM.alter upd k o
    where 
      upd :: Maybe Value -> Maybe Value
      upd _ = Just a
