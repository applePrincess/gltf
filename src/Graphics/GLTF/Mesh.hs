{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Graphics.GLTF.Mesh
  ( Mesh(..)
  , Primitive(..)
  , pattern Points       
  , pattern Lines        
  , pattern LineLoop     
  , pattern LineStrip    
  , pattern Triangles    
  , pattern TriangleStrip
  , pattern TriangleFan  
  ) where

  import Data.List.NonEmpty (NonEmpty)
  import Data.Maybe
  import GHC.Generics

  import Data.Aeson
  import Data.Aeson.Types (Parser)
  import Data.Scientific (Scientific)

  import Graphics.GLTF.Type

  -- | A set of primitives to be rendered.  A node can contain one mesh.  A node's transform places the mesh in the scene.
  data Mesh = Mesh
    { primitives :: NonEmpty Primitive -- ^ An array of primitives, each defining geometry to be rendered with a material.
    , weights :: Maybe (NonEmpty Scientific) -- ^ Array of weights to be applied to the Morph Targets.
    , name :: Maybe Name
    , extensions :: Maybe Extension
    , extras :: Maybe Extras
    } deriving (Generic, Show)
  instance FromJSON Mesh where

  -- | Geometry to be rendered with the given material.
  data Primitive = Primitive
    { attributes :: JSONObject -- ^ A dictionary object, where each key corresponds to mesh attribute semantic and each value is the index of the accessor containing attribute's data.
    , indices :: Maybe GLTFID -- ^ The index of the accessor that contains the indices.
    , material :: Maybe GLTFID -- ^ The index of the material to apply to this primitive when rendering.
    , mode :: PrimitiveMode -- ^ The type of primitives to render.
    , targets :: Maybe (NonEmpty GLTFID) -- ^ An array of Morph Targets, each  Morph Target is a dictionary mapping attributes (only `POSITION`, `NORMAL`, and `TANGENT` supported) to their deviations in the Morph Target.
    , extensions :: Maybe Extension
    , extras :: Maybe Extras 
    } deriving (Generic, Show)
  instance FromJSON Primitive where
    parseJSON = withObject "Primitive" $ \obj -> Primitive
      <$> obj .: "attributes"
      <*> obj .:? "indices"
      <*> obj .:? "material"
      <*> obj .:? "mode" .!= Triangles
      <*> obj .:? "targets"
      <*> obj .:? "extensions"
      <*> obj .:? "extras"

  newtype PrimitiveMode = PrimitiveMode Type deriving newtype FromJSON
                                             deriving stock   Show
  pattern Points        = PrimitiveMode (Type 0)
  pattern Lines         = PrimitiveMode (Type 1)
  pattern LineLoop      = PrimitiveMode (Type 2)
  pattern LineStrip     = PrimitiveMode (Type 3)
  pattern Triangles     = PrimitiveMode (Type 4)
  pattern TriangleStrip = PrimitiveMode (Type 5)
  pattern TriangleFan   = PrimitiveMode (Type 6)
  
