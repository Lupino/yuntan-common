module Crypto.Signature
  ( signParams
  , signJSON
  , hmacSHA256
  , signRaw

  , mkHexHash
  , signParams_
  , signJSON_
  , signRaw_
  ) where

import           Crypto.Hash            (Digest, SHA256)
import           Crypto.MAC             (HMAC (..), hmac)
import           Data.Aeson             (Value (..))
import           Data.Aeson.Key         (Key, toText)
import           Data.Aeson.KeyMap      (KeyMap)
import qualified Data.Aeson.KeyMap      as KeyMap (toList)
import           Data.Byteable          (toBytes)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Char8  as B (ByteString, concat, empty, pack,
                                              unpack)
import           Data.CaseInsensitive   (CI, mk)
import           Data.List              (sortOn)
import           Data.Scientific        (Scientific, floatingOrInteger)
import           Data.Text.Encoding     (encodeUtf8)
import qualified Data.Text.Lazy         as LT (Text, toStrict, unpack)
import qualified Data.Vector            as V (Vector, toList)

-- | Make a case-insensitive hex hash string by hmac sha256
hmacSHA256 :: B.ByteString -> B.ByteString -> CI B.ByteString
hmacSHA256 solt = mkHexHash (mkHmacSHA256Hash solt)

mkHmacSHA256Hash :: B.ByteString -> B.ByteString -> Digest SHA256
mkHmacSHA256Hash solt = hmacGetDigest . hmac solt

-- | Make a case-insensitive hex hash string by a hash function
mkHexHash :: (B.ByteString -> Digest a) -> B.ByteString -> CI B.ByteString
mkHexHash mkHash = mk . B16.encode . toBytes . mkHash

sortAndJoinTextParams :: [(LT.Text, LT.Text)] -> B.ByteString
sortAndJoinTextParams = join . sort
  where sort :: [(LT.Text, LT.Text)] -> [(LT.Text, LT.Text)]
        sort = sortOn (\(k, _) -> LT.unpack k)

        join :: [(LT.Text, LT.Text)] -> B.ByteString
        join ((k,v):xs) = B.concat [encodeUtf8 $ LT.toStrict k, encodeUtf8 $ LT.toStrict v, join xs]
        join []         = B.empty

-- | Sign a text params use a hash function
signParams_ :: (B.ByteString -> Digest a) -> [(LT.Text, LT.Text)] -> CI B.ByteString
signParams_ mkHash = mkHexHash mkHash . sortAndJoinTextParams

-- | Sign a text params use hmac sha256
signParams :: B.ByteString -> [(LT.Text, LT.Text)] -> CI B.ByteString
signParams solt = signParams_ (mkHmacSHA256Hash solt)

sortAndJoinJSON :: Value -> B.ByteString
sortAndJoinJSON = v2b
  where sortKeyMap :: KeyMap Value -> [(Key, Value)]
        sortKeyMap = sortOn fst . KeyMap.toList

        joinList :: [(Key, Value)] -> B.ByteString
        joinList []          = B.empty
        joinList ((k, v):xs) = B.concat [encodeUtf8 $ toText k, v2b v, joinList xs]

        joinArray :: V.Vector Value -> B.ByteString
        joinArray = B.concat . map v2b . V.toList

        v2b :: Value -> B.ByteString
        v2b (Object v)   = (joinList . sortKeyMap) v
        v2b (Array v)    = joinArray v
        v2b (String v)   = encodeUtf8 v
        v2b (Number v)   = B.pack $ showNumber v
        v2b (Bool True)  = B.pack "true"
        v2b (Bool False) = B.pack "false"
        v2b Null         = B.empty

        showNumber :: Scientific -> String
        showNumber v = case floatingOrInteger v of
                         Left n  -> show n
                         Right n -> show n

-- | Sign JSON data use a hash function
signJSON_ :: (B.ByteString -> Digest a) -> Value -> CI B.ByteString
signJSON_ mkHash = mkHexHash mkHash . sortAndJoinJSON

-- | Sign JSON data use hmac sha256
signJSON :: B.ByteString -> Value -> CI B.ByteString
signJSON solt = signJSON_ (mkHmacSHA256Hash solt)

sortAndJoinRawParams :: [(B.ByteString, B.ByteString)] -> B.ByteString
sortAndJoinRawParams = join . sort
  where sort :: [(B.ByteString, B.ByteString)] -> [(B.ByteString, B.ByteString)]
        sort = sortOn (\(k, _) -> B.unpack k)

        join :: [(B.ByteString, B.ByteString)] -> B.ByteString
        join []         = B.empty
        join ((k,v):xs) = B.concat [k, v, join xs]

-- | Sign bytestring params use a hash function
signRaw_ :: (B.ByteString -> Digest a) -> [(B.ByteString, B.ByteString)] -> CI B.ByteString
signRaw_ mkHash = mkHexHash mkHash . sortAndJoinRawParams

-- | Sign bytestring params use hmac sha256
signRaw :: B.ByteString -> [(B.ByteString, B.ByteString)] -> CI B.ByteString
signRaw solt = signRaw_ (mkHmacSHA256Hash solt)
