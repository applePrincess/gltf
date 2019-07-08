{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GLTF.BufferView where

  import GHC.Generics
  import Numeric.Natural
  
  import Data.Aeson

  import Graphics.GLTF.Type
  import Graphics.GLTF.Validation

  -- |  A view into a buffer generally representing a subset of the buffer.
  data BufferView = BufferView
    { buffer :: GLTFID -- ^ The index of the buffer.
    , byteOffset :: Natural -- ^ The offset into the buffer in bytes.
    , byteLength :: Integer -- ^ The total byte length of the buffer view.
    , byteStride :: Maybe Integer -- ^ The stride, in bytes.
    , target :: TargetType -- ^ The target that the GPU buffer should be bound to.
    -- , name, extensions, extras
    } deriving (Generic, Show)
  instance FromJSON BufferView where
    parseJSON = withObject "BufferView" $ \obj -> BufferView
      <$> obj .: "buffer"
      <*> (obj .:? "byteOffset" >>= validateMaybe (validateMin "byteOffset" 0 False)) .!= 0
      <*> (obj .: "byteLength" >>= validateByteLength)
      <*> (obj .:? "byteStride" >>= validateMaybe validateByteStride)
      <*> obj .: "target"
      where validateByteStride n = if n < 4 || n > 252 || n `mod` 4 /= 0
                                   then fail "byteStride must be in range [4, 252] and multiple of 4"
                                   else return n

  newtype TargetType = TargetType String deriving newtype FromJSON
                                         deriving stock   Show