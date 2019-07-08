{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GLTF.Texture where
  import GHC.Generics

  import Data.Aeson

  import Graphics.GLTF.Type

  -- | A texture and its sampler.
  data Texture = Texture
    { sampler :: Maybe GLTFID -- ^ The index of the sampler used by this texture. When undefined, a sampler with repeat wrapping and auto filtering should be used.
    , source :: Maybe GLTFID -- ^ The index of the image used by this texture. When undefined, it is expected that an extension or other mechanism will supply an alternate texture source, otherwise behavior is undefined.
    -- name, extensions, extras
    } deriving (Generic, Show)
  instance FromJSON Texture where
    
