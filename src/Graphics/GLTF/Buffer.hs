{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GLTF.Buffer where
  
  import GHC.Generics

  import Data.Aeson
  
  import Graphics.GLTF.Validation

  -- | A buffer points to binary geometry, animation, or skins.
  data Buffer = Buffer
    { uri :: Maybe String -- ^ The uri of the buffer.
    , byteLength :: Integer -- ^ The length of the buffer in bytes.
    } deriving (Generic, Show)
  instance FromJSON Buffer where
    parseJSON = withObject "Buffer" $ \obj -> Buffer
      <$> obj .:? "uri"
      <*> (obj .: "byteLength" >>= validateByteLength)