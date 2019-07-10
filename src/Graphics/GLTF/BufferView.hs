{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.GLTF.BufferView
  ( BufferView(..)
  , TargetType
  , pattern ArrayBuffer
  , pattern ElementArrayBuffer
  ) where

  import Control.Monad
  import GHC.Generics
  import Numeric.Natural
  
  import Data.Aeson

  import Graphics.GLTF.Type

  -- |  A view into a buffer generally representing a subset of the buffer.
  data BufferView = BufferView
    { buffer :: GLTFID -- ^ The index of the buffer.
    , byteOffset :: Natural -- ^ The offset into the buffer in bytes.
    , byteLength :: ByteLength -- ^ The total byte length of the buffer view.
    , byteStride :: Maybe Integer -- ^ The stride, in bytes.
    , target :: TargetType -- ^ The target that the GPU buffer should be bound to.
    , name :: Maybe Name
    , extensions :: Maybe Extension
    , extras :: Maybe Extras
    } deriving (Generic, Show)
  instance FromJSON BufferView where
    parseJSON = withObject "BufferView" $ \obj -> BufferView
      <$> obj .: "buffer"
      <*> obj .:? "byteOffset" .!= 0
      <*> obj .: "byteLength"
      <*> (obj .:? "byteStride" >>= mapM validateByteStride)
      <*> obj .: "target"
      <*> obj .:? "name"
      <*> obj .:? "extensions"
      <*> obj .:? "extras"
      where validateByteStride n = if n < 4 || n > 252 || n `mod` 4 /= 0
                                   then fail "byteStride must be in range [4, 252] and multiple of 4"
                                   else return n

  newtype TargetType = TargetType Type deriving newtype FromJSON
                                         deriving stock   Show
  -- | ARRAY_BUFFER
  pattern ArrayBuffer = TargetType 34962
  -- | ELEMENT_ARRAY_BUFFER
  pattern ElementArrayBuffer = TargetType 34963