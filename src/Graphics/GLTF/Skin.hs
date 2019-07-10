{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GLTF.Skin
  (Skin(..)
  ) where

  import Data.List.NonEmpty
  import GHC.Generics

  import Data.Aeson

  import Graphics.GLTF.Type
  import Graphics.GLTF.Validation
  
  -- | Joints and matrices defining a skin.
  data Skin = Skin
    { inverseBindMatrices :: Maybe GLTFID -- ^ The index of the accessor containing the floating-point 4x4 inverse-bind matrices.  The default is that each matrix is a 4x4 identity matrix, which implies that inverse-bind matrices were pre-applied.
    , skeleton :: Maybe GLTFID -- ^ The index of the node used as a skeleton root.
    , joints :: NonEmpty GLTFID -- ^ Indices of skeleton nodes, used as joints in this skin.
    , name :: Maybe Name
    , extensions :: Maybe Extension
    , extras :: Maybe Extras
    } deriving (Generic, Show)
  instance FromJSON Skin where
