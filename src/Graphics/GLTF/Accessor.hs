{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.GLTF.Accessor
  ( Accessor(..)
  , Sparse(..)
  , SparseIndex(..)
  , SparseValue(..)
  , pattern Byte         
  , pattern UnsignedByte 
  , pattern Short        
  , pattern UnsignedShort
  , pattern UnsignedInt  
  , pattern Float_
  , pattern Scalar
  , pattern Vec2  
  , pattern Vec3  
  , pattern Vec4  
  , pattern Mat2  
  , pattern Mat3  
  , pattern Mat4
  ) where

  import GHC.Generics
  import Numeric.Natural

  import Data.Aeson

  import Graphics.GLTF.Type
  import Graphics.GLTF.Validation

  -- | A typed view into a bufferView.  A bufferView contains raw binary data.  
  --   An accessor provides a typed view into a bufferView or a subset of a 
  --   bufferView similar to how WebGL's `vertexAttribPointer()` defines an 
  --   attribute in a buffer.
  data Accessor = Accessor
    { bufferView :: Maybe GLTFID -- ^ The index of the bufferView.
    , byteOffset :: Natural -- ^ The offset relative to the start of the bufferView in bytes.
    , componentType :: ComponentType -- ^ The datatype of components in the attribute.
    , normalized :: Bool -- ^ Specifies whether integer data values should be normalized.
    , count :: Integer -- ^ The number of attributes referenced by this accessor.
    , ttype :: AccessorType -- ^ Specifies if the attribute is a scalar, vector, or matrix.
    , max :: Maybe [Double] -- ^ Maximum value of each component in this attribute.
    , min :: Maybe [Double] -- ^ Minimum value of each component in this attribute.
    , sparse :: Maybe Sparse -- ^ Sparse storage of attributes that deviate from their initialization value.
    -- name, extensions, extras
    } deriving (Eq, Generic, Show)
  instance FromJSON Accessor where
    parseJSON = withObject "Accessor" $ \obj -> Accessor
      <$> obj .:? "bufferView"
      <*> obj .:? "byteOffset" .!= 0
      <*> obj .: "componentType"
      <*> obj .:? "normalized" .!= False
      <*> (obj .: "count" >>= validateCount)
      <*> obj .: "type"
      <*> (obj .:? "max" >>= validateRange)
      <*> (obj .:? "min" >>= validateRange)
      <*> obj .:? "sparse"
  
  -- | Sparse storage of attributes that deviate from their initialization value.
  data Sparse = Sparse
    { count :: Integer -- ^ Number of entries stored in the sparse array.
    , indices :: SparseIndex -- ^ Index array of size `count` that points to those accessor attributes that deviate from their initialization value. Indices must strictly increase.
    , values :: SparseValue  -- ^ Array of size `count` times number of components, storing the displaced accessor attributes pointed by `indices`. Substituted values must have the same `componentType` and number of components as the base accessor.
    -- extensions, extras
    } deriving (Eq, Generic, Show)
  instance FromJSON Sparse where
    parseJSON = withObject "Sparse" $ \obj -> Sparse
      <$> (obj .: "count" >>= validateCount)
      <*> obj .: "indices"
      <*> obj .: "values"

  -- | Indices of those attributes that deviate from their initialization value.
  data SparseIndex = SparseIndex
    { bufferView :: GLTFID -- ^ The index of the bufferView with sparse indices. Referenced bufferView can't have ARRAY_BUFFER or ELEMENT_ARRAY_BUFFER target.
    , byteOffset :: Natural -- ^ The offset relative to the start of the bufferView in bytes. Must be aligned.
    , componentType :: ComponentType -- ^ The indices data type.
--    , extensioins :: JSONObject
--    , extras :: JSONObject
    } deriving (Eq, Generic, Show)
  instance FromJSON SparseIndex where
    parseJSON = withObject "SparseIndex" $ \obj -> SparseIndex
      <$> obj .: "bufferView"
      <*> obj .: "byteOffset" .!= 0
      <*> obj .: "componentType"

  -- | Array of size `accessor.sparse.count` times number of components storing 
  -- | the displaced accessor attributes pointed by `accessor.sparse.indices`.
  data SparseValue = SparseValue
    { bufferView :: GLTFID -- ^ The index of the bufferView with sparse values. Referenced bufferView can't have ARRAY_BUFFER or ELEMENT_ARRAY_BUFFER target.
    , byteOffset :: Natural -- ^ The offset relative to the start of the bufferView in bytes. Must be aligned.
--    , extensions :: JSONObject
--    , extras :: JSONObject 
    } deriving (Eq, Generic, Show)
  instance FromJSON SparseValue where
    parseJSON = withObject "SparseValue" $ \obj -> SparseValue
      <$> obj .: "bufferView"
      <*> obj .:? "byteOffset" .!= 0
  
  newtype ComponentType = ComponentType Type deriving newtype (Eq, FromJSON)
                                             deriving stock   (Show)
  pattern Byte          = ComponentType  (Type 5120) 
  pattern UnsignedByte  = ComponentType (Type 5121)
  pattern Short         = ComponentType (Type 5122)
  pattern UnsignedShort = ComponentType (Type 5123)
  pattern UnsignedInt   = ComponentType (Type 5125)
  pattern Float_        = ComponentType (Type 5126)
  newtype AccessorType = AccessorType String deriving newtype (Eq, FromJSON)
                                             deriving stock   (Show)
  pattern Scalar = AccessorType "SCALAR"
  pattern Vec2   = AccessorType "VEC2"
  pattern Vec3   = AccessorType "VEC3"
  pattern Vec4   = AccessorType "VEC4"
  pattern Mat2   = AccessorType "MAT2"
  pattern Mat3   = AccessorType "MAT3"
  pattern Mat4   = AccessorType "MAT4"