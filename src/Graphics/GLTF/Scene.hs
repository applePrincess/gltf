{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GLTF.Scene
  (Scene(..)
  ) where

  import Data.List.NonEmpty
  import GHC.Generics

  import Data.Aeson

  import Graphics.GLTF.Type
  import Graphics.GLTF.Validation

  -- | The root nodes of a scene.
  data Scene = Scene
    { nodes :: Maybe (NonEmpty GLTFID)  --  ^ The indices of each root node.
    , name :: Maybe Name
    , extensions :: Maybe Extension
    , extras :: Maybe Extras
    } deriving (Generic, Show)
  
  instance FromJSON Scene where
