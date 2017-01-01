module Dispatch.Utils.Signature
  (
    signParams
  , signJSON
  , hmacMD5
  ) where

import           Crypto.Hash               (MD5)
import           Crypto.MAC                (HMAC (..), hmac)
import           Data.Aeson                (Value (..))
import           Data.Byteable             (toBytes)
import qualified Data.ByteString.Char8     as B (ByteString, append, concat,
                                                 empty, pack)
import qualified Data.HashMap.Lazy         as LH (HashMap, toList)
import           Data.Hex                  (hex)
import           Data.List                 (sortOn)
import qualified Data.Text                 as T (Text, unpack)
import qualified Data.Text.Lazy            as LT (Text, unpack)
import qualified Data.Vector               as V (Vector, toList)
import           Web.Scotty.Internal.Types (Param)

t2b :: LT.Text -> B.ByteString
t2b = B.pack . LT.unpack

t2b' :: T.Text -> B.ByteString
t2b' = B.pack . T.unpack

hmacMD5 :: B.ByteString -> B.ByteString -> B.ByteString
hmacMD5 solt = h2b . hmac solt

h2b :: HMAC MD5 -> B.ByteString
h2b = toBytes . hmacGetDigest

signParams :: B.ByteString -> [Param] -> B.ByteString
signParams solt = hex . hmacMD5 solt . joinParams . sortParams
  where sortParams :: [Param] -> [Param]
        sortParams params = sortOn (\(k, _) -> LT.unpack k) params

        joinParams :: [Param] -> B.ByteString
        joinParams ((k,v):xs) = t2b k `B.append` t2b v `B.append` joinParams xs
        joinParams [] = B.empty

signJSON :: B.ByteString -> Value -> B.ByteString
signJSON solt = hex . hmacMD5 solt . v2b
  where sortHashMap :: LH.HashMap T.Text Value -> [(T.Text, Value)]
        sortHashMap = sortOn (\(k, _) -> T.unpack k) . LH.toList

        joinList :: [(T.Text, Value)] -> B.ByteString
        joinList []          = B.empty
        joinList ((k, v):xs) = t2b' k `B.append` v2b v `B.append` joinList xs

        joinArray :: V.Vector Value -> B.ByteString
        joinArray = B.concat . map v2b . V.toList

        v2b :: Value -> B.ByteString
        v2b (Object v)   = (joinList . sortHashMap) v
        v2b (Array v)    = joinArray v
        v2b (String v)   = t2b' v
        v2b (Number v)   = B.pack $ show v
        v2b (Bool True)  = B.pack "true"
        v2b (Bool False) = B.pack "false"
        v2b Null         = B.empty
