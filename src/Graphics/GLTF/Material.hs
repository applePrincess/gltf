{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Graphics.GLTF.Material
  ( Material(..)
  , NormalTextureInfo(..)
  , OcclusionTextureInfo(..)
  , PBRMaterialRoughness(..)
  , pattern Opaque
  , pattern Mask
  , pattern Blend
  ) where

  import Data.Maybe
  import GHC.Generics
  import Numeric.Natural

  import Data.Aeson
  import Data.Scientific (Scientific)
  import qualified Data.Vector.Sized as VS

  import Graphics.GLTF.TextureInfo
  import Graphics.GLTF.Type

  -- The material appearance of a primitive.
  data Material = Material 
    { pbrMaterialRoughness :: Maybe PBRMaterialRoughness -- ^ A set of parameter values that are used to define the metallic-roughness material model from Physically-Based Rendering (PBR) methodology. When not specified, all the default values of `pbrMetallicRoughness` apply.
    , normalTexture :: Maybe NormalTextureInfo -- ^ The normal map texture.
    , occlusionTexture :: Maybe OcclusionTextureInfo -- ^ The occlusion map texture.
    , emissiveTexture :: Maybe TextureInfo -- ^ The emissive map texture.
    , emissiveFactor :: SizedVec 3 (ZToP1) -- ^ The emissive color of the material.
    , alphaMode :: AlphaMode -- ^ The alpha rendering mode of the material.
    , alphaCutoff :: AlphaCutoff -- ^ The alpha cutoff value of the material.
    , doubleSided :: Bool -- ^ Specifies whether the material is double sided.
    , name :: Maybe Name
    , extensions :: Maybe Extension
    , extras :: Maybe Extras
    } deriving (Generic, Show)
  instance FromJSON Material where
    parseJSON = withObject "Material" $ \obj -> Material
      <$> obj .:? "pbrMaterialRoughness"
      <*> obj .:? "normalTexture"
      <*> obj .:? "occlusionTexture"
      <*> obj .:? "emissiveTexture"
      <*> obj .:? "emissiveFactor" .!= SizedVec (fromJust $ VS.fromList [0.0, 0.0, 0.0])
      <*> obj .:? "alphaMode" .!= Opaque
      <*> obj .:? "alphaCutoff" .!= 0.5
      <*> obj .:? "doubleSided" .!= False
      <*> obj .:? "name"
      <*> obj .:? "extensions"
      <*> obj .:? "extras"

  -- | Material Normal Texture Info
  data NormalTextureInfo = NormalTextureInfo 
    { scale :: Scientific -- ^ The scalar multiplier applied to each normal vector of the normal texture.
    , index :: Natural -- ^ The index of the texture.
    , texCoord :: Natural -- ^ The set index of texture's TEXCOORD attribute used for texture coordinate mapping.
    , extensions :: Maybe Extension
    , extras :: Maybe Extras
    } deriving (Generic, Show)
  instance FromJSON NormalTextureInfo where
    parseJSON = withObject "NormalTextureInfo" $ \obj -> NormalTextureInfo
      <$> obj .:? "scale" .!= 1.0
      <*> obj .: "index"
      <*> obj .:? "texCoord" .!= 0
      <*> obj .:? "extensions"
      <*> obj .:? "extras"

  data OcclusionTextureInfo = OcclusionTextureInfo
    { strength :: NNFloat -- ^ A scalar multiplier controlling the amount of occlusion applied.
    , index :: Natural -- ^ The index of the texture.
    , texCoord :: Natural -- ^ The set index of texture's TEXCOORD attribute used for texture coordinate mapping.
    , extensions :: Maybe Extension
    , extras :: Maybe Extras
    } deriving (Generic, Show)
  instance FromJSON OcclusionTextureInfo where
    parseJSON = withObject "OcclusionTextureInfo" $ \obj -> OcclusionTextureInfo
      <$> obj .:? "strength" .!= 1.0
      <*> obj .: "index"
      <*> obj .:? "texCoord" .!= 0
      <*> obj .:? "extensions"
      <*> obj .:? "extras"

  -- | A set of parameter values that are used to define the metallic-roughness material model from Physically-Based Rendering (PBR) methodology.
  data PBRMaterialRoughness = PBRMaterialRoughness
    { baseColorFactor :: SizedVec 4 (M1ToP1) -- ^ The material's base color factor.
    , baseColorTexture :: Maybe TextureInfo -- ^ The base color texture.
    , metallicFactor :: M1ToP1 -- ^ The metalness of the material.
    , roughnessFactor :: M1ToP1 -- ^ The roughness of the material.
    , metallicRoughnessTexture :: Maybe TextureInfo -- ^ The metallic-roughness texture.
    , extensions :: Maybe Extension
    , extras :: Maybe Extras
    } deriving (Generic, Show)
  instance FromJSON PBRMaterialRoughness where
    parseJSON = withObject "PBRMaterialRoughness" $ \obj -> PBRMaterialRoughness
      <$> obj .:? "baseColorFactor" .!= SizedVec (fromJust $ VS.fromList [1.0, 1.0, 1.0, 1.0])
      <*> obj .:? "baseColorTexture"
      <*> obj .:? "metallicFactor" .!= 1.0
      <*> obj .:? "roughnessFactor" .!= 1.0
      <*> obj .:? "metallicRoughnessTexture"
      <*> obj .:? "extensions"
      <*> obj .:? "extras"

  newtype AlphaCutoff = AlphaCutoff NNFloat deriving newtype (FromJSON, Num, Fractional, Ord, Eq)
                                            deriving stock   (Show)
  newtype AlphaMode = AlphaMode String deriving newtype (FromJSON)
                                       deriving stock   (Show)
  -- | The alpha value is ignored and the rendered output is fully opaque.
  pattern Opaque = AlphaMode "OPAQUE"
  -- | The rendered output is either fully opaque or fully transparent depending on the alpha value and the specified alpha cutoff value.
  pattern Mask   = AlphaMode "MASK"
  -- | The alpha value is used to composite the source and destination areas. The rendered output is combined with the background using the normal painting operation (i.e. the Porter and Duff over operator).
  pattern Blend  = AlphaMode "BLEND"
  