{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Graphics.GLTF.Animation
  ( Animation(..)
  , Channel(..)
  , Sampler(..)
  , Target(..)
  , pattern Linear      
  , pattern Step        
  , pattern CubicSpline
  , pattern Translation
  , pattern Rotation   
  , pattern Scale      
  , pattern Weights    
  ) where

  import Data.List.NonEmpty (NonEmpty)
  import GHC.Generics

  import Data.Aeson

  import Graphics.GLTF.Type

  -- | A keyframe animation.
  data Animation = Animation
    { channels :: NonEmpty Channel -- ^ An array of channels, each of which targets an animation's sampler at a node's property. Different channels of the same animation can't have equal targets.
    , samplers :: NonEmpty Sampler -- ^ An array of samplers that combines input and output accessors with an interpolation algorithm to define a keyframe graph (but not its target).
    , name :: Maybe Name
    , extensions :: Maybe Extension
    , extras :: Maybe Extras 
    } deriving (Eq, Generic, Show)
  instance FromJSON Animation where

  -- | Targets an animation's sampler at a node's property.
  data Channel = Channel
    { sampler :: GLTFID -- ^ The index of a sampler in this animation used to compute the value for the target.
    , target :: Target -- ^ The index of the node and TRS property to target.
    , extensions :: Maybe Extension
    , extras :: Maybe Extras 
    } deriving (Eq, Generic, Show)
  instance FromJSON Channel where

  -- | Combines input and output accessors with an interpolation algorithm to define a keyframe graph (but not its target).
  data Sampler = Sampler
    { input :: GLTFID -- ^ The index of an accessor containing keyframe input values, e.g., time.
    , intrepolation :: Interpolation -- ^ Interpolation algorithm.
    , output :: GLTFID -- ^ The index of an accessor, containing keyframe output values.
    , extensions :: Maybe Extension
    , extras :: Maybe Extras 
    } deriving (Eq, Generic, Show)
  instance FromJSON Sampler where
    parseJSON = withObject "Sampler" $ \obj -> Sampler
      <$> obj .: "input"
      <*> obj .:? "interpolation" .!= Linear
      <*> obj .: "output"
      <*> obj .:? "extensions"
      <*> obj .:? "extras"

  -- | The index of the node and TRS property that an animation channel targets.
  data Target = Target
    { node :: Maybe GLTFID -- ^ The index of the node to target.
    , path :: Path -- ^ The name of the node's TRS property to modify, or the "widgets" of the Morph Targets it instanciates.For the \"translation\" property, the values that are provided by the sampler are the translation along the x, y, and z axes. For the \"rotation\" property, the values are a quaternion in the order (x, y, z, w), where w is the scalar. For the \"scale\" property, the values are the scaling factors along the x, y, and z axes."
    , extensions :: Maybe Extension
    , extras :: Maybe Extras 
    } deriving (Eq, Generic, Show)
  instance FromJSON Target where
  
  newtype Interpolation = Interpolation String deriving newtype (Eq, FromJSON)
                                               deriving stock   (Show)
  pattern Linear      = Interpolation "LINEAR"
  pattern Step        = Interpolation "STEP"
  pattern CubicSpline = Interpolation "CUBICSPLINE"

  newtype Path = Path String deriving newtype (Eq, FromJSON)
                             deriving stock   (Show)
  pattern Translation = Path "translation"
  pattern Rotation    = Path "rotation"
  pattern Scale       = Path "scale"
  pattern Weights     = Path "weights"
