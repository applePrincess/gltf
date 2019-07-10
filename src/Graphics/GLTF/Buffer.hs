{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GLTF.Buffer
  ( Buffer(..)
  ) where
  
  import GHC.Generics
  import Numeric.Natural

  import Data.Aeson
  
  import Graphics.GLTF.Type

  -- | A buffer points to binary geometry, animation, or skins.
  data Buffer = Buffer
    { uri :: Maybe String -- ^ The uri of the buffer.
    , byteLength :: ByteLength -- ^ The length of the buffer in bytes.
    , name :: Maybe Name
    , extensions :: Maybe Extension
    , extras :: Maybe Extras
    } deriving (Generic, Show)
  instance FromJSON Buffer where

-- todo: positive integer type.