{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-} 

module Text.JSONb.Layer.Reader where

import Control.Applicative
import "monads-tf" Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.Trie as Trie

import Text.JSONb (JSON(..))
import qualified Text.JSONb as JSONb


data JsContext =
  JsContext { jscJSON        :: !JSON
            , jscCurrentPath :: !JsPath }
  
defaultContext :: JsContext  
defaultContext = JsContext JSONb.Null []
  
inField :: JsField -> JSON -> JsContext -> JsContext
inField field js ctxt = JsContext { jscJSON = js
                                  , jscCurrentPath = field : jscCurrentPath ctxt }


type JsReader a = ReaderT JsContext JsResult a

getJson :: JsReader JSON
getJson = asks jscJSON

getPath :: JsReader JsPath
getPath = asks jscCurrentPath

jsError :: String -> JsReader a
jsError err = do
  context <- ask
  lift $ JsError context err

jsNotFound :: JsField -> JsReader a
jsNotFound pathItem = do
  cp <- asks jscCurrentPath
  lift $ JsNotFound (pathItem : cp)

ifNotFound :: JsReader a -> a -> JsReader a
ifNotFound jsrd a = do
  context <- ask
  case runReaderT jsrd context of
    ok@(JsOk _)  -> lift ok
    JsNotFound _ -> return a
    err          -> lift err

whenNotFound :: b -> JsReader b -> JsReader b
whenNotFound = flip ifNotFound


data JsResult a = JsOk a
                | JsError JsContext String
                | JsNotFound JsPath

data JsField = JsIndex !Int | JsField !ByteString

instance Monad JsResult where
    JsError path err >>= _f = JsError path err
    JsNotFound nf >>= _f = JsNotFound nf
    (JsOk x) >>= f = f x
    
    return = JsOk
    fail s = JsError defaultContext s

instance Functor JsResult where
    fmap = liftM

instance Applicative JsResult where
    pure  = return
    (<*>) = ap

instance MonadPlus JsResult where
    mzero = JsError defaultContext ""
    j1 `mplus` j2 = do
      case j1 of
        JsOk _ -> j1
        _      -> j2

instance Alternative JsResult where
    empty = mzero
    (<|>) = mplus

type JsPath = [JsField]

showPath :: JsPath -> String
showPath path =
  case reverse path of
    [] -> "No path"
    (object:fields) -> concat $ showObject object : map showField fields
  where
    showObject (JsField f) = B.unpack f
    showObject (JsIndex i) = show i
    showField (JsIndex i) = concat ["[", show i, "]"]
    showField (JsField f) = concat ["->", B.unpack f]

runJsReader :: JsReader a -> JSON -> Either String a
runJsReader reader json = case runReaderT reader (JsContext json []) of
    JsOk x              -> Right x
    JsError context msg -> do
      let errMsg = case jscCurrentPath context of
            [] -> msg
            path  -> concat ["Failed reading at path \"", showPath path, "\": ", msg]
      Left errMsg
    JsNotFound path ->
      Left $ "Could not get the following field: " ++ showPath path

lookupJs :: ByteString -> JsReader a -> JsReader a
lookupJs fieldStr js = do
  let field = JsField fieldStr
  json <- getJson
  case json of
    Object obj ->
        case Trie.lookup fieldStr obj of          
          Just a  -> local (inField field a) $ js
          Nothing -> jsNotFound field
    KeyValues keyvals ->
        case lookup fieldStr keyvals of
          Just a  -> local (inField field a) $ js
          Nothing -> jsNotFound field
    _ -> jsNotFound field

infixr 9 ~>
(~>) :: ByteString -> JsReader a -> JsReader a
(~>) = lookupJs
