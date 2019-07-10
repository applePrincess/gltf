{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GLTF.Sampler
  ( Sampler(..)
  , MagFilter
  , pattern NearestMag
  , pattern LinearMag
  , MinFilter
  , pattern NearestMin          
  , pattern LinearMin
  , WrappingMode           
  , pattern NearestMipmapNearest
  , pattern LinearMipmapNearest 
  , pattern NearestMipmapLinear 
  , pattern LinearMipmapLinear
  , pattern ClampToEdge   
  , pattern MirroredRepeat
  , pattern Repeat
  ) where

  import GHC.Generics

  import Data.Aeson

  import Graphics.GLTF.Type

  -- | Texture sampler properties for filtering and wrapping modes.
  data Sampler = Sampler
    { magFilter :: Maybe MagFilter -- ^ Magnification filter.
    , minFilter :: Maybe MinFilter -- ^ Minification filter.
    , wrapS :: WrappingMode -- ^ s wrapping mode.
    , wrapT :: WrappingMode -- ^ t wrapping mode.
    , name :: Maybe Name
    , extensions :: Maybe Extension
    , extras :: Maybe Extras
    } deriving (Generic, Show)
  instance FromJSON Sampler where
    parseJSON = withObject "Sampler" $ \obj -> Sampler
      <$> obj .:? "magFilter"
      <*> obj .:? "minFilter"
      <*> obj .:? "wrapS" .!= Repeat
      <*> obj .:? "wrapT" .!= Repeat
      <*> obj .:? "name"
      <*> obj .:? "extensions"
      <*> obj .:? "extras"
  newtype MagFilter = MagFilter Type deriving newtype (FromJSON)
                                     deriving stock   (Show, Generic)
  pattern NearestMag = MagFilter (Type 9728)
  pattern LinearMag  = MagFilter (Type 9729) 
  newtype MinFilter = MinFilter Type deriving newtype (FromJSON)
                                     deriving stock   (Show, Generic)
  pattern NearestMin           = MinFilter (Type 9728)
  pattern LinearMin            = MinFilter (Type 9729)
  pattern NearestMipmapNearest = MinFilter (Type 9984)
  pattern LinearMipmapNearest  = MinFilter (Type 9985)
  pattern NearestMipmapLinear  = MinFilter (Type 9986)
  pattern LinearMipmapLinear   = MinFilter (Type 9987)
  newtype WrappingMode = WrappingMode Type deriving newtype (FromJSON)
                                           deriving stock (Show, Generic)
  pattern ClampToEdge    = WrappingMode (Type 33071)
  pattern MirroredRepeat = WrappingMode (Type 33648)
  pattern Repeat         = WrappingMode (Type 10497)