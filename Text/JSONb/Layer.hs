{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.JSONb.Layer
    (
      ToJSON(..)
    , FromJSON(..)
      
    , BiJSON
      
    , JsReader
    , runJsReader
      
    , JsResult(..)
    , JsContext(..)
      
    , toJson
    , toJsonObj
    , toJsonArr
      
    , fromJson
    , readJson
      
    , jsError
    , jsNotFound
    , (~>)
    , whenNotFound
    , ifNotFound
      
    , readJsonByteStringWith
    , readJsonByteString
      
    , readJsonFileWith
    , readJsonFile
      
    , fromJsonWithReadClass
      
    , fromJsonArray
      
    , maybeToJson
    , maybeToJsonWith      
    , maybeFromJson
    ) where

import Control.Applicative
import Data.Ratio
import "monads-tf" Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import Text.JSONb (JSON)
import qualified Text.JSONb as JSONb

import Text.JSONb.Layer.Reader


------------------------------------------------------------------------------
-- * Main classes
------------------------------------------------------------------------------

-- | Conversion from Haskell to JSON

class ToJSON env a where
    toJsonWith :: env -> a -> JSON

-- | Conversion from @JSON@ to Haskell
-- 
-- You only need to provide one of the two methods,
-- @fromJsonWith@ and @readJsonWith@.

class FromJSON env a where
    -- | Conversion using the JsReader monad
    fromJsonWith :: env -> JsReader a
    fromJsonWith env = do
      json <- getJson
      case readJsonWith env json of
        Right x  -> return x
        Left err -> jsError err
    
    -- | Basic conversion, directly inspects a @JSON@ value and returns in @Either@
    readJsonWith :: env -> JSON -> Either String a
    readJsonWith env = runJsReader (fromJsonWith env)

-- | Bidirectional json class

class (ToJSON e1 a, FromJSON e2 a) => BiJSON e1 e2 a
instance (ToJSON e1 a, FromJSON e2 a) => BiJSON e1 e2 a


------------------------------------------------------------------------------
-- * Encoding functions
------------------------------------------------------------------------------

toJson :: ToJSON () a => a -> JSON
toJson = toJsonWith ()

-- | Make a json object
toJsonObj :: [(ByteString, JSON)] -> JSON
toJsonObj = JSONb.KeyValues

toJsonArr :: [JSON] -> JSON
toJsonArr = JSONb.Array


------------------------------------------------------------------------------
-- * Decoding functions
------------------------------------------------------------------------------

-- ** Simple decoding functions

fromJson :: FromJSON () a => JsReader a    
fromJson = fromJsonWith ()

readJson :: FromJSON () a => JSON -> Either String a
readJson = runJsReader fromJson

readJsonByteStringWith :: FromJSON env a => env -> ByteString -> Either String a
readJsonByteStringWith env str = readJsonWith env =<< JSONb.decode str

readJsonByteString :: FromJSON () a => ByteString -> Either String a
readJsonByteString = readJsonByteStringWith ()

readJsonFileWith :: FromJSON env a => env -> FilePath -> IO (Either String a)
readJsonFileWith env file = readJsonByteStringWith env <$> B.readFile file

readJsonFile :: FromJSON () a => FilePath -> IO (Either String a)
readJsonFile = readJsonFileWith ()


------------------------------------------------------------------------------
-- * Encoding/Decoding helper functions
------------------------------------------------------------------------------

-- ** Decoding with @Read@ instance

-- | Reads a value from a @JSON@ string using its @Read@ instance
fromJsonWithReadClass :: Read a => JsReader a
fromJsonWithReadClass = f =<< getJson
    where f (JSONb.String s)
              | Just x <- maybeRead (B.unpack s) = return x
              | otherwise                        = jsError "Wrong string format"
          f _ = jsError "Not a json string"

-- ** Decoding an array of json values

-- | Reads an array of values with the provided JsReader value
fromJsonArray :: JsReader a -> JsReader [a]
fromJsonArray reader = do
  json <- getJson
  case json of
    JSONb.Array xs ->
      mapM (\(i,js) -> local (inField (JsIndex i) js) reader) $ zip [1..] xs
    _        ->
      jsError "Not an array"

-- ** Encoding and decoding with @Maybe@

maybeToJson :: ToJSON () a => Maybe a -> JSON
maybeToJson Nothing  = JSONb.Null
maybeToJson (Just a) = toJson a

maybeToJsonWith :: ToJSON env a => env -> Maybe a -> JSON
maybeToJsonWith _env Nothing = JSONb.Null
maybeToJsonWith env (Just a) = toJsonWith env a

maybeFromJson :: JsReader a -> JsReader (Maybe a)
maybeFromJson reader = do
  context <- ask
  case runReaderT reader context of
    JsOk a          -> return (Just a)
    JsNotFound path -> lift $ JsNotFound path
    JsError errCtx errMsg ->    
      case jscJSON errCtx of
        JSONb.Null -> return Nothing
        _          -> lift $ JsError errCtx errMsg


-----------------------------------------------------------------------------
-- * Instances
------------------------------------------------------------------------------

-- ** Lists

instance FromJSON env a => FromJSON env [a] where
  fromJsonWith env = fromJsonArray (fromJsonWith env)

instance ToJSON env a => ToJSON env [a] where
  toJsonWith env xs = JSONb.Array (map (toJsonWith env) xs)

-- ** Booleans

instance ToJSON () Bool where
  toJsonWith () = JSONb.Boolean

instance FromJSON () Bool where
  readJsonWith () (JSONb.Boolean b) = return b
  readJsonWith () _                 = Left "Not a boolean"

-- ** Strings

instance ToJSON () ByteString where
  toJsonWith () = JSONb.String

instance FromJSON () ByteString where
  readJsonWith () (JSONb.String s) = return s
  readJsonWith () _          = Left "Not a string"
              
instance ToJSON () String where
  toJsonWith () = JSONb.String . B.pack
  
instance FromJSON () String where
  readJsonWith () js = B.unpack <$> readJsonWith () js

-- ** Numerals

instance Integral a => ToJSON () a where
  toJsonWith () = JSONb.Number . (% 1) . fromIntegral

-- TODO: Replace by some sort of TypeError ...
instance (Num a, Read a) => FromJSON () a where
  readJsonWith () json = do
    case json of
      JSONb.Number ratio -> Right $ fromInteger $ numerator ratio `div` denominator ratio
      JSONb.Boolean   _ -> Left $ "Found boolean instead of number"
      JSONb.Null        -> Left $ "Found null instead of number"
      JSONb.Array     _ -> Left $ "Found array instead of number"
      JSONb.Object    _ -> Left $ "Found object instead of number"
      JSONb.KeyValues _ -> Left $ "Found object instead of number"
      JSONb.String str   ->
        case maybeRead (B.unpack str) of
          Just i  -> Right i
          Nothing -> Left $ "Not a number: " ++ B.unpack str

-- * Helpers

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = case reads s of
        [(x, [])] -> Just x
        _         -> Nothing
