{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GLTF.TextureInfo
  ( TextureInfo(..)
  ) where

  import GHC.Generics
  import Numeric.Natural

  import Data.Aeson

  import Graphics.GLTF.Type
  import Graphics.GLTF.Validation

  -- | Reference to a texture.
  data TextureInfo = TextureInfo
    { index :: GLTFID -- ^ The index of the texture.
    , texCoord :: Natural -- ^ The set index of texture's TEXCOORD attribute used for texture coordinate mapping.
    , extensions :: Maybe Extension
    , extras :: Maybe Extras
    } deriving (Generic, Show)

  instance FromJSON TextureInfo where
    parseJSON = withObject "TextureInfo" $ \obj -> TextureInfo
      <$> obj .: "index"
      <*> obj .:? "texCoord" .!= 0
      <*> obj .:? "extensions"
      <*> obj .:? "extras"
