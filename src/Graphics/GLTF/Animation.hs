{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GLTF.Animation where

  import GHC.Generics

  import Data.Aeson

  import Graphics.GLTF.Type

  -- | A keyframe animation.
  data Animation = Animation
    { channels :: [Channel] -- ^ An array of channels, each of which targets an animation's sampler at a node's property. Different channels of the same animation can't have equal targets.
    , samplers :: [AnimationSampler] -- ^ An array of samplers that combines input and output accessors with an interpolation algorithm to define a keyframe graph (but not its target).
    -- , name, extensions, extras 
    } deriving (Eq, Generic, Show)
  instance FromJSON Animation where

  -- | Targets an animation's sampler at a node's property.
  data Channel = Channel
    { sampler :: GLTFID -- ^ The index of a sampler in this animation used to compute the value for the target.
    , target :: Target -- ^ The index of the node and TRS property to target.
    } deriving (Eq, Generic, Show)
  instance FromJSON Channel where

  -- | Combines input and output accessors with an interpolation algorithm to define a keyframe graph (but not its target).
  data AnimationSampler = AnimationSampler
    { input :: GLTFID -- ^ The index of an accessor containing keyframe input values, e.g., time.
    , intrepolation :: Interpolation -- ^ Interpolation algorithm.
    , output :: GLTFID -- ^ The index of an accessor, containing keyframe output values.
    } deriving (Eq, Generic, Show)
  instance FromJSON AnimationSampler where
    parseJSON = withObject "AnimationSampler" $ \obj -> AnimationSampler
      <$> obj .: "input"
      <*> obj .:? "interpolation" .!= LinearInterpolation
      <*> obj .: "output"

  -- | The index of the node and TRS property that an animation channel targets.
  data Target = Target
    { node :: Maybe GLTFID -- ^ The index of the node to target.
    , path :: Path -- ^ The name of the node's TRS property to modify, or the "widgets" of the Morph Targets it instanciates.For the \"translation\" property, the values that are provided by the sampler are the translation along the x, y, and z axes. For the \"rotation\" property, the values are a quaternion in the order (x, y, z, w), where w is the scalar. For the \"scale\" property, the values are the scaling factors along the x, y, and z axes."
    -- extensions, extras
    } deriving (Eq, Generic, Show)
  instance FromJSON Target where
  
  newtype Interpolation = Interpolation String deriving newtype (Eq, FromJSON)
                                               deriving stock   (Show)
  pattern LinearInterpolation = Interpolation "LINEAR"
  newtype Path = Path String deriving newtype (Eq, FromJSON)
                             deriving stock   (Show)
