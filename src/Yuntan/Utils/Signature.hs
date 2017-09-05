module Yuntan.Utils.Signature
  (
    signParams
  , signJSON
  , hmacSHA256
  , signRaw
  ) where

import           Crypto.Hash           (SHA256)
import           Crypto.MAC            (HMAC (..), hmac)
import           Data.Aeson            (Value (..))
import           Data.Byteable         (toBytes)
import qualified Data.ByteString.Char8 as B (ByteString, concat, empty, pack,
                                             unpack)
import qualified Data.HashMap.Lazy     as LH (HashMap, toList)
import           Data.Hex              (hex)
import           Data.List             (sortOn)
import qualified Data.Text             as T (Text, unpack)
import           Data.Text.Encoding    (encodeUtf8)
import qualified Data.Text.Lazy        as LT (Text, toStrict, unpack)
import qualified Data.Vector           as V (Vector, toList)

hmacSHA256 :: B.ByteString -> B.ByteString -> B.ByteString
hmacSHA256 solt = h2b . hmac solt

h2b :: HMAC SHA256 -> B.ByteString
h2b = toBytes . hmacGetDigest

signParams :: B.ByteString -> [(LT.Text, LT.Text)] -> B.ByteString
signParams solt = hex . hmacSHA256 solt . join . sort
  where sort :: [(LT.Text, LT.Text)] -> [(LT.Text, LT.Text)]
        sort = sortOn (\(k, _) -> LT.unpack k)

        join :: [(LT.Text, LT.Text)] -> B.ByteString
        join ((k,v):xs) = B.concat [encodeUtf8 $ LT.toStrict k, encodeUtf8 $ LT.toStrict v, join xs]
        join []         = B.empty

signJSON :: B.ByteString -> Value -> B.ByteString
signJSON solt = hex . hmacSHA256 solt . v2b
  where sortHashMap :: LH.HashMap T.Text Value -> [(T.Text, Value)]
        sortHashMap = sortOn (\(k, _) -> T.unpack k) . LH.toList

        joinList :: [(T.Text, Value)] -> B.ByteString
        joinList []          = B.empty
        joinList ((k, v):xs) = B.concat [encodeUtf8 k, v2b v, joinList xs]

        joinArray :: V.Vector Value -> B.ByteString
        joinArray = B.concat . map v2b . V.toList

        v2b :: Value -> B.ByteString
        v2b (Object v)   = (joinList . sortHashMap) v
        v2b (Array v)    = joinArray v
        v2b (String v)   = encodeUtf8 v
        v2b (Number v)   = B.pack . num $ show v
        v2b (Bool True)  = B.pack "true"
        v2b (Bool False) = B.pack "false"
        v2b Null         = B.empty

        num :: String -> String
        num []         = []
        num ['.', '0'] = []
        num (x:xs)     = x : num xs

signRaw :: B.ByteString -> [(B.ByteString, B.ByteString)] -> B.ByteString
signRaw solt = hex . hmacSHA256 solt . join . sort
  where sort :: [(B.ByteString, B.ByteString)] -> [(B.ByteString, B.ByteString)]
        sort = sortOn (\(k, _) -> B.unpack k)

        join :: [(B.ByteString, B.ByteString)] -> B.ByteString
        join []         = B.empty
        join ((k,v):xs) = B.concat [k, v, join xs]