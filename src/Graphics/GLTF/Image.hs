{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.GLTF.Image
  ( Image(..)
  , MimeType(..)
  , pattern JPEG
  , pattern PNG
  ) where
  import Control.Monad
  import Data.Maybe
  import GHC.Generics

  import Data.Aeson 

  import Graphics.GLTF.Type

  -- ^ Image data used to create a texture. Image can be referenced by URI or `bufferView` index. `mimeType` is required in the latter case.
  data Image = Image
    { uri :: Maybe String -- ^ The uri of the image.
    , mimeType :: MimeType -- ^ The image's MIME type. Required if `bufferView` is defined.
    , bufferView :: Maybe GLTFID -- ^ The index of the bufferView that contains the image. Use this instead of the image's uri property.
    , name :: Maybe Name
    , extensions :: Maybe Extension
    , extras :: Maybe Extras
    } deriving (Generic, Show)
  instance FromJSON Image where
    parseJSON = withObject "Image" $ \obj -> do
      uri <- obj .:? "uri"
      mimeType <- obj .: "mimeType"
      bufferView <- obj .:? "bufferView"
    
      when ((isNothing uri && isNothing bufferView) ||
            (isJust uri && isJust bufferView)) $ fail "Excactly one of uri or bufferView can only be present."
      Image uri mimeType bufferView <$> obj .:? "name" <*> obj .:? "extensions" <*> obj .:? "extras"
  -- uri and bufferView cannnot be Nothing at the same time. 

  newtype MimeType = MimeType String deriving newtype (FromJSON)
                                     deriving stock   (Show)
  pattern JPEG = MimeType "image/jpeg"
  pattern PNG  = MimeType "image/png" 