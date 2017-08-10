{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (pack)
import qualified GHC.Generics as GHC
import GHC.TypeLits
import Generics.SOP
import Generics.SOP.Record

data MyRecord = MyRecord
  { a :: Int
  , b :: String
  , c :: Bool
  } deriving (Show, GHC.Generic)
instance Generic MyRecord
instance HasDatatypeInfo MyRecord

-- BORING MANUAL INSTANCE LAND
-- instance FromJSON MyRecord where
--   parseJSON = withObject "MyRecord" go
--     where
--       go o = MyRecord
--         <$> o .: "a"
--         <*> o .: "b"
--         <*> o .: "c"

instance FromJSON MyRecord where
  parseJSON = withObject "MyRecord" go
    where
      go o = do
        record <- parseRecordFields (Proxy @ MyRecord) o
        pure $ fromRecord record

parseRecordFields :: forall f a xs
   . IsRecord a xs
  => ExtractFields xs
  => f a -> Object -> Parser (Record xs)
parseRecordFields _ = extractFields (Proxy @ xs)

class ExtractFields (xs :: RecordCode) where
  extractFields :: forall f
     . f xs -> Object -> Parser (Record xs)

instance
  ( ExtractFields xs
  , KnownSymbol name
  , FromJSON ty
  ) => ExtractFields ('(name, ty) : xs) where
  extractFields _ o = do
    field :: ty <- o .: pack (symbolVal $ Proxy @ name)
    rest <- extractFields (Proxy @ xs) o
    pure $ P field :* rest

instance ExtractFields '[] where
  extractFields _ _ = pure Nil

main :: IO ()
main = do
  let record :: MyRecord = fromRecord $ P 1 :* (P "b" :* (P True :* Nil))
  print record
  print $ toRecord (MyRecord 1 "a" True)
  putStrLn "dsdfsdf"
  let decoded :: Maybe MyRecord = decode "{\"a\": 1, \"b\": \"b\", \"c\": false}"
  print decoded
