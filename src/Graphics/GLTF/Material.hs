{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.GLTF.Material where

  import Data.Maybe
  import GHC.Generics
  import Numeric.Natural

  import Data.Aeson

  import Graphics.GLTF.TextureInfo
  import Graphics.GLTF.Type
  import Graphics.GLTF.Validation

  -- The material appearance of a primitive.
  data Material = Material 
    { pbrMaterialRoughness :: Maybe PBRMaterialRoughness -- ^ A set of parameter values that are used to define the metallic-roughness material model from Physically-Based Rendering (PBR) methodology. When not specified, all the default values of `pbrMetallicRoughness` apply.
    , normalTexture :: Maybe NormalTextureInfo -- ^ The normal map texture.
    , occlusionTexture :: Maybe OcclusionTextureInfo -- ^ The occlusion map texture.
    , emissiveTexture :: Maybe TextureInfo -- ^ The emissive map texture.
    , emissiveFactor :: [Double] -- ^ The emissive color of the material.
    , alphaMode :: AlphaMode -- ^ The alpha rendering mode of the material.
    , alphaCutoff :: AlphaCutoff -- ^ The alpha cutoff value of the material.
    , doubleSided :: Bool -- ^ Specifies whether the material is double sided.
    -- name, extensions, extras
    } deriving (Generic, Show)
  instance FromJSON Material where
    parseJSON = withObject "Material" $ \obj -> Material
      <$> obj .:? "pbrMaterialRoughness"
      <*> obj .:? "normalTexture"
      <*> obj .:? "occlusionTexture"
      <*> obj .:? "emissiveTexture"
      <*> (obj .:? "emissiveFactor" >>= validateEmissiveFactor) .!= [0.0, 0.0, 0.0]
      <*> obj .:? "alphaMode" .!= Opaque
      <*> (obj .:? "alphaCutoff" >>= validateAlphaCutoff) .!= AlphaCutoff 0.5
      <*> obj .:? "doubleSided" .!= False
      where validateEmissiveFactor o = if all (\x -> x >= 0 && x <= 1) (fromJust o) && 
                                          length o == 3
                                       then return o
                                       else fail "emissiveFactor must be in range [0.0, 1.0]"
            validateAlphaCutoff = validateMaybe (validateMin "alphaCutoff" 0.0 False)
  
  -- | Material Normal Texture Info
  data NormalTextureInfo = NormalTextureInfo 
    { scale :: Double -- ^ The scalar multiplier applied to each normal vector of the normal texture.
    , index :: Natural -- ^ The index of the texture.
    , texCoord :: Natural -- ^ The set index of texture's TEXCOORD attribute used for texture coordinate mapping.
    -- extensions, extras
    } deriving (Generic, Show)
  instance FromJSON NormalTextureInfo where
    parseJSON = withObject "NormalTextureInfo" $ \obj -> NormalTextureInfo
      <$> obj .:? "scale" .!= 1.0
      <*> obj .: "index"
      <*> obj .:? "texCoord" .!= 0

  data OcclusionTextureInfo = OcclusionTextureInfo
    { strength :: Double -- ^ A scalar multiplier controlling the amount of occlusion applied.
    , index :: Natural -- ^ The index of the texture.
    , texCoord :: Natural -- ^ The set index of texture's TEXCOORD attribute used for texture coordinate mapping.
  -- extensions, extras
    } deriving (Generic, Show)
  instance FromJSON OcclusionTextureInfo where
    parseJSON = withObject "OcclusionTextureInfo" $ \obj -> OcclusionTextureInfo
      <$> (obj .:? "strength" >>= validateMaybe (inRange 0.0 1.0)) .!= 1.0
      <*> obj .: "index"
      <*> obj .:? "texCoord" .!= 0

  -- | A set of parameter values that are used to define the metallic-roughness material model from Physically-Based Rendering (PBR) methodology.
  data PBRMaterialRoughness = PBRMaterialRoughness
    { baseColorFactor :: [Double] -- ^ The material's base color factor.
    , baseColorTexture :: Maybe TextureInfo -- ^ The base color texture.
    , metallicFactor :: Double -- ^ The metalness of the material.
    , roughnessFactor :: Double -- ^ The roughness of the material.
    , metallicRoughnessTexture :: Maybe TextureInfo -- ^ The metallic-roughness texture.
    -- extensions, extras
    } deriving (Generic, Show)
  instance FromJSON PBRMaterialRoughness where
    parseJSON = withObject "PBRMaterialRoughness" $ \obj -> PBRMaterialRoughness
      <$> (obj .:? "baseColorFactor" >>= validateMaybe validateBaseColor) .!= [1.0, 1.0, 1.0, 1.0]
      <*> obj .:? "baseColorTexture"
      <*> (obj .:? "metallicFactor" >>= validateMaybe (inRange 0.0 1.0)) .!= 1.0
      <*> (obj .:? "roughnessFactor" >>= validateMaybe (inRange 0.0 1.0)) .!= 1.0
      <*> obj .:? "metallicRoughnessTexture"
      where validateBaseColor xs = if length xs == 4 && all (\x -> 0.0 <= x  && x <= 1.0) xs
                                   then return xs
                                   else fail "baseColorFactor must be length of 4 and every element must be in range [0.0, 1.0]"


  newtype AlphaCutoff = AlphaCutoff Double deriving newtype (FromJSON, Num, Fractional, Ord, Eq)
                                           deriving stock   (Show)
  newtype AlphaMode = AlphaMode String deriving newtype (FromJSON)
                                       deriving stock   (Show)
  -- | The alpha value is ignored and the rendered output is fully opaque.
  pattern Opaque = AlphaMode "OPAQUE"
  -- | The rendered output is either fully opaque or fully transparent depending on the alpha value and the specified alpha cutoff value.
  pattern Mask = AlphaMode "MASK"
  -- | The alpha value is used to composite the source and destination areas. The rendered output is combined with the background using the normal painting operation (i.e. the Porter and Duff over operator).
  pattern Blend = AlphaMode "BLEND"
  
