{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GLTF.Scene where

  import GHC.Generics

  import Data.Aeson

  import Graphics.GLTF.Type
  import Graphics.GLTF.Validation

  -- | The root nodes of a scene.
  data Scene = Scene
    { nodes :: Maybe [GLTFID]  --  ^ The indices of each root node.
    -- , name, extensions, extras
    } deriving (Generic, Show)
  
  instance FromJSON Scene where
    parseJSON = withObject "Scene" $ \obj -> Scene
      <$> (obj .:? "nodes" >>= validateMaybe (validateLength 1))
