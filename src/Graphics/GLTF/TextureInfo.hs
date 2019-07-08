{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GLTF.TextureInfo where

  import GHC.Generics

  import Data.Aeson

  import Graphics.GLTF.Type
  import Graphics.GLTF.Validation

  -- | Reference to a texture.
  data TextureInfo = TextureInfo
    { index :: GLTFID -- ^ The index of the texture.
    , texCoord :: Integer -- ^ The set index of texture's TEXCOORD attribute used for texture coordinate mapping.
    -- extensions, extras
    } deriving (Generic, Show)

  instance FromJSON TextureInfo where
    parseJSON = withObject "TextureInfo" $ \obj -> TextureInfo
      <$> obj .: "index"
      <*> (obj .:? "texCoord" >>= validateMaybe (validateMin "texCoord" 0 False)) .!= 0
