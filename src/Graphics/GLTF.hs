{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.GLTF where

import Control.Monad.ST
import Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.STRef
import Data.Maybe (catMaybes)
import Data.String (IsString)
import qualified Data.Text as T
import Data.Word (Word32)
import GHC.Generics

import Data.Aeson
import Data.Aeson.Types (Pair)

data GLTF = GLTF
  { extensionsUsed :: Maybe [GLTFString]
  , extensionsRequired :: Maybe [GLTFString]
  , accessors :: Maybe [Accessor]
  , animations :: Maybe [Animation]
  , asset :: Asset
  , buffers :: Maybe [Buffer]
  , bufferView :: Maybe [BufferView]
  , cameras :: Maybe [Camera]
  , images :: Maybe [Image]
  , materials :: Maybe [Material]
  , meshes :: Maybe [Mesh]
  , nodes :: Maybe [Node]
  , samples :: Maybe [Sampler]
  , scene :: Maybe Int
  , skins :: Maybe [Skin]
  , textures :: Maybe [Texture]
  , extensions :: Maybe Object
  , extras :: Maybe Object
  } deriving (Generic)
instance FromJSON GLTF
instance ToJSON GLTF

data Scene = Scene
  { nodes :: Maybe [NodeIndex]
  , name :: Maybe GLTFString
  , extensions :: Maybe Object
  , extras :: Maybe Object
  } deriving (Generic)

instance ToJSON Scene
instance FromJSON Scene

data Node = Node
  { camera :: Maybe CameraIndex
  , children :: Maybe [NodeIndex]
  , skin :: Maybe SkinIndex
  , matrix :: Matrix -- defaults to [0,0,0,0,0,0...]
  , mesh :: Maybe MeshIndex
  , rotation :: (GLTFFloat, GLTFFloat, GLTFFloat, GLTFFloat) -- defaults to [0,0,0,1]
  , scale :: (GLTFFloat, GLTFFloat, GLTFFloat) -- defaults to [1,1,1]
  , translation :: (GLTFFloat, GLTFFloat, GLTFFloat) -- defaults to [0,0,0]
  , weights :: Maybe [GLTFFloat]
  , name :: Maybe GLTFString
  , extensions :: Maybe Object
  , extras  :: Maybe Object
  } 

defaultMatrix :: Matrix
defaultMatrix = [1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1]

defaultRotation :: (GLTFFloat, GLTFFloat, GLTFFloat, GLTFFloat)
defaultRotation = (0,0,0,1)

defaultScale :: (GLTFFloat, GLTFFloat, GLTFFloat)
defaultScale = (1,1,1)

defaultTranslation :: (GLTFFloat, GLTFFloat, GLTFFloat)
defaultTranslation = (0, 0, 0)

instance FromJSON Node where
  parseJSON = withObject "Node" $ \kvp -> Node
    <$> kvp .:? "camera"
    <*> kvp .:? "children"
    <*> kvp .:? "skin"
    <*> kvp .:? "matrix" .!= defaultMatrix
    <*> kvp .:? "mesh"
    <*> kvp .:? "rotation" .!= defaultRotation 
    <*> kvp .:? "scale" .!= defaultScale
    <*> kvp .:? "translation" .!= defaultTranslation
    <*> kvp .:? "weights"
    <*> kvp .:? "name"
    <*> kvp .:? "extensions"
    <*> kvp .:? "extras"

instance ToJSON Node where
  toJSON Node{..} = object kvp
    where
      kvp = catMaybes [ toKVP "camera" camera
                      , toKVP "children" children
                      , toKVP "skin" skin
                      , toKVP "matrix" (if matrix == defaultMatrix then Nothing else Just matrix)
                      , toKVP "mesh" mesh
                      , toKVP "rotation" (if rotation == defaultRotation then Nothing else Just rotation)
                      , toKVP "scale" (if scale == defaultScale then Nothing else Just scale)
                      , toKVP "translation" (if translation == defaultTranslation then Nothing else Just scale)
                      , toKVP "weights" weights
                      , toKVP "name" name
                      , toKVP "extensions" extensions
                      , toKVP "extras" extras
                      ]

data Skin = Skin
  { inverseBindMatrices :: Maybe Int
  , skeleton :: Maybe Int
  , joints :: [Int]
  , name :: Maybe GLTFString
  , extensions :: Maybe Object
  , extras :: Maybe Object
  } deriving (Generic)

instance ToJSON Skin
instance FromJSON Skin

data Mesh = Mesh
 { primitives :: [Primitive]
 , weights :: Maybe [GLTFFloat]
 , name :: Maybe GLTFString
 , extensions :: Maybe Object
 , extras :: Maybe Object
 } deriving (Generic)

instance ToJSON Mesh
instance FromJSON Mesh

data Accessor = Accessor
  { bufferView :: BufferView
  , byteOffset :: Offset -- defaults 0
  , componentType :: ComponentType
  , normalized :: Bool -- defaults false
  , count      :: Integer
  , accessorType :: AccessorType
  , maxRange   :: Maybe Int -- maps to accessor.max, rename to avoid collision to Prelude.max
  , minRange   :: Maybe Int --         accessor.min                               Prelude.min
  , sparse     :: Maybe Sparse
  , name       :: GLTFString
  , extensions :: Object
  , extras     :: Object
  }
instance FromJSON Accessor where
  parseJSON = withObject "accessor" \obj -> Accessor
    <$> obj .: "bufferView"
    <*> obj .:? "byteOffset" .!= 0
    <*> obj .: "componentType"
    <*> obj .:? "normalized" .!= False
    <*> obj .: "count"
    <*> obj .: "accessorType"
    <*> obj .:? "max"
    <*> obj .:? "min"
    <*> obj .:? "sparse"
    <*> obj .:? "name" .!= mempty
    <*> obj .:? "extensions" .!= mempty
    <*> obj .:? "extras" .!= mempty

instance ToJSON Accessor where
  toJSON Accessor{..} = object kvp
    where 
      kvp = [ "bufferView" .= bufferView
            , "componentType" .= componentType
            , "max" .= maxRange
            , "min" .= minRange
            , "sparse" .= sparse
            ] ++
            catMaybes
              [ toKVP "byteOffset" (if byteOffset == 0 then Nothing else Just byteOffset)
              , toKVP "normalized" (if normalized then Just True else Nothing)
              , toKVP "name" (if gltfStringNull name then Nothing else Just name)
              , toKVP "extensions" (if null extensions then Nothing else Just extensions)
              , toKVP "extras" (if null extras then Nothing else Just extras)
              ]

data Material = Material
  { name :: GLTFString
  , extensions :: Object
  , pbrMetalicRoughness :: Maybe PBRParam
  , normalTexture :: Maybe NormalTexture
  , occlusionTexture :: Maybe OcclusionTexture
  , emissiveTexture :: Maybe EmissiveTexture
  , emissiveFactor :: (Int, Int, Int) -- detfaults to (0, 0, 0)
  , alphaMode :: AlphaMode -- defaults to OPAQUE
  , alphaCutoff :: GLTFFloat -- may be float better?, defaults to 0.5
  , doubleSided :: Bool -- defaults to false
  }

instance FromJSON Material where
  parseJSON = withObject "material" $ \obj -> Material
    <$> obj .:? "name" .!= mempty
    <*> obj .:? "extensions" .!= mempty
    <*> obj .:? "pbrMetalicRoughness"
    <*> obj .:? "normalTexture"
    <*> obj .:? "occulusionTexture"
    <*> obj .:? "emissiveTexture"
    <*> obj .:? "emissiveFactor" .!= (0, 0, 0)
    <*> obj .:? "alphaMode" .!= OPAQUE
    <*> obj .:? "alphaCutoff" .!= 0.5
    <*> obj .:? "doubleSided" .!= False

instance ToJSON Material where
  toJSON Material{..} = object kvp
    where
      kvp = [ "pbrMetalicRoughness" .= pbrMetalicRoughness
            , "normalTexture" .= normalTexture
            , "occlusionTexture" .= occlusionTexture
            , "emissiveTexture" .= emissiveTexture
            ] ++ catMaybes
              [ toKVP "emissiveFactor" (if emissiveFactor == (0, 0, 0) then Nothing else Just emissiveFactor)
              , toKVP "alphaMode" (if alphaMode == OPAQUE then Nothing else Just alphaMode)
              , toKVP "alphaCutoff" (if alphaCutoff == 0.5 then Nothing else Just alphaCutoff)
              , toKVP "doubleSided" (if doubleSided then Nothing else Just doubleSided)
              , toKVP "name"  (if gltfStringNull name then Nothing else Just name)
              , toKVP "extensions" (if null extensions then Nothing else Just extensions)
              ]

data PBRParam = PBRParam
  { baseColorFactor :: (GLTFFloat, GLTFFloat, GLTFFloat, GLTFFloat) -- defaults to (1, 1, 1, 1)
  , baseColorTexture :: Maybe Texture
  , metallicFactor :: GLTFFloat -- defaults to 1
  , roughnessFactor :: GLTFFloat -- defaults to 1
  , metalicRoughnessTexture :: Maybe Texture
  , extensions :: Object
  , extras  :: Object
  }

instance FromJSON PBRParam where
  parseJSON = withObject "pbrparam" $ \obj -> PBRParam
    <$> obj .:? "baseColorFactor" .!= (1, 1, 1, 1)
    <*> obj .:? "baseColorTexture"
    <*> obj .:? "metallicFactor" .!= 1
    <*> obj .:? "rouchnessFactor" .!= 1
    <*> obj .:? "metalicRoughnessTexture"
    <*> obj .: "extensions" .!= mempty
    <*> obj .: "extras" .!= mempty

instance ToJSON PBRParam where
  toJSON PBRParam{..} = object kvp
    where 
      kvp = catMaybes 
              [ toKVP "baseColorFactor" (if baseColorFactor == (1,1,1,1) then Nothing else Just baseColorFactor)
              , toKVP "baseColorTexture" baseColorTexture
              , toKVP "metallicFactor" (if metallicFactor == 1 then Nothing else Just metallicFactor)
              , toKVP "roughnessFactor" (if roughnessFactor == 1 then Nothing else Just roughnessFactor)
              , toKVP "extensions" (if null extensions then Nothing else Just extensions)
              , toKVP "extras" (if null extras then Nothing else Just extras)
              ]

data NormalTexture = NormalTexture 
  { index :: Int
  , texCoord :: Int -- defaults to 0
  , scale :: GLTFFloat -- defaults to 1
  , extensions :: Object
  , extras  :: Object
  }

instance FromJSON NormalTexture where
  parseJSON = withObject "normaltexture" $ \obj -> NormalTexture
    <$> obj .: "index"
    <*> obj .:? "texCoord" .!= 0
    <*> obj .:? "scale" .!= 1
    <*> obj .:? "extensions" .!= mempty
    <*> obj .:? "extras" .!= mempty

instance ToJSON NormalTexture where
  toJSON NormalTexture{..} = object kvp
    where
      kvp = ["index" .= index] ++
        catMaybes
          [ toKVP "texCoord" (if texCoord == 0 then Nothing else Just texCoord)
          , toKVP "scale" (if scale == 1 then Nothing else Just scale)
          , toKVP "extensions" (if null extensions then Nothing else Just extensions)
          , toKVP "extras" (if null extras then Nothing else Just extras)
          ]

data OcclusionTexture = OcclusionTexture
  { index :: Int
  , texCoord :: Int -- defaults to 0
  , strength :: GLTFFloat -- defaults to 1
  , extensions :: Object
  , extras  :: Object
  }

instance FromJSON OcclusionTexture where
  parseJSON = withObject "normaltexture" $ \obj -> OcclusionTexture
    <$> obj .: "index"
    <*> obj .:? "texCoord" .!= 0
    <*> obj .:? "strength" .!= 1
    <*> obj .:? "extensions" .!= mempty
    <*> obj .:? "extras" .!= mempty

instance ToJSON OcclusionTexture where
  toJSON OcclusionTexture{..} = object kvp
    where
      kvp = ["index" .= index] ++
        catMaybes
          [ toKVP "texCoord" (if texCoord == 0 then Nothing else Just texCoord)
          , toKVP "strength" (if strength == 1 then Nothing else Just strength)
          , toKVP "extensions" (if null extensions then Nothing else Just extensions)
          , toKVP "extras" (if null extras then Nothing else Just extras)
          ]


data EmissiveTexture = EmissiveTexture
  { index :: Int
  , texCoord :: Int -- defaults to 0
  , extensions :: Object
  , extras  :: Object
  }

instance FromJSON EmissiveTexture where
  parseJSON = withObject "normaltexture" $ \obj -> EmissiveTexture
    <$> obj .: "index"
    <*> obj .:? "texCoord" .!= 0
    <*> obj .:? "extensions" .!= mempty
    <*> obj .:? "extras" .!= mempty

instance ToJSON EmissiveTexture where
  toJSON EmissiveTexture{..} = object kvp
    where
      kvp = ["index" .= index] ++
        catMaybes
          [ toKVP "texCoord" (if texCoord == 0 then Nothing else Just texCoord)
          , toKVP "extensions" (if null extensions then Nothing else Just extensions)
          , toKVP "extras" (if null extras then Nothing else Just extras)
          ]

data Texture = Texture
  { sampler :: Maybe SamplerIndex
  , source :: Maybe SourceIndex
  , name :: Maybe GLTFString
  , extensions :: Maybe Object
  , extras :: Maybe Object
  } deriving (Generic)
instance FromJSON Texture
instance ToJSON Texture

type ByteOffset = Word
type SamplerIndex = Int
type SourceIndex = Int

data Image = Image
  { uri :: Maybe GLTFString
  , mimeType :: Maybe MimeType
  , bufferView :: Maybe BufferView
  , name :: Maybe GLTFString
  , extensions :: Maybe Object
  , extras  :: Maybe Object
  } deriving (Generic)
instance FromJSON Image
instance ToJSON Image

data Indices = Indices
  { bufferView :: Int
  , byteOffset :: Int -- defaults to 0
  , componentType :: ComponentType
  , extensions :: Object
  , extras :: Object
  }

data Sampler = Sampler
  { magFilter :: Maybe MagnitudeFilter
  , minFilter :: Maybe MinificationFilter
  , wrapS :: Wrapping --defaults to REPEAT
  , wrapT :: Wrapping -- defaults to REPEAT
  }

instance FromJSON Sampler where
  parseJSON = withObject "Sampler" $ \kvp -> Sampler
    <$> kvp .:? "magFilter"
    <*> kvp .:? "minFilter"
    <*> kvp .:? "wrapS" .!= REPEAT
    <*> kvp .:? "wrapT" .!= REPEAT

instance ToJSON Sampler where
  toJSON Sampler{..} = object kvp
    where
      kvp = catMaybes [ toKVP "magFilter" magFilter
                      , toKVP "minFilter" minFilter
                      , toKVP "wrapS" (if wrapS == REPEAT then Nothing else Just wrapS)
                      , toKVP "wrapT" (if wrapT == REPEAT then Nothing else Just wrapT)
                      ]
    

type MagnitudeFilter = Int
type MinificationFilter = Int
pattern NEAREST :: MagnitudeFilter
pattern NEAREST = 9728
pattern LINEAR :: MagnitudeFilter
pattern LINEAR  = 9729
pattern NEAREST_MIPMAP_NEAREST :: MinificationFilter
pattern NEAREST_MIPMAP_NEAREST = 9984 
pattern LINEAR_MIPMAP_NEAREST :: MinificationFilter
pattern LINEAR_MIPMAP_NEAREST = 9985 
pattern NEAREST_MIPMAP_LINEAR :: MinificationFilter
pattern NEAREST_MIPMAP_LINEAR = 9986 
pattern LINEAR_MIPMAP_LINEAR :: MinificationFilter
pattern LINEAR_MIPMAP_LINEAR = 9987 

type Wrapping = Int
pattern CLAMP_TO_EDGE :: Wrapping
pattern CLAMP_TO_EDGE = 33071
pattern MIRRORED_REPEAT :: Wrapping
pattern MIRRORED_REPEAT = 33648
pattern REPEAT :: Wrapping
pattern REPEAT = 10497

data GLB = GLB
  { version :: Word32
  , textual :: GLTF
  , binary  :: B.ByteString -- null binary <==> absense of binary buffer
  }

toWord32 :: BL.ByteString -> Word32
toWord32 src = (fromIntegral b1) .|.
               (fromIntegral b2 `shiftL` 8) .|.
               (fromIntegral b3 `shiftL` 16) .|.
               (fromIntegral b4 `shiftL` 24)
  where src' = BL.unpack src
        b1 = src' !! 0
        b2 = src' !! 1
        b3 = src' !! 2
        b4 = src' !! 3

parseGLB :: BL.ByteString -> Either String GLB
parseGLB src = runST do
  src' <- newSTRef src
  magic <- BL.take 4 <$> readSTRef src'
  modifySTRef src' (BL.drop 4)
  version <- toWord32 . BL.take 4 <$> readSTRef src'
  modifySTRef src' (BL.drop 4)
  -- length <- take 4 <$> readSTRef src'
  modifySTRef src' (BL.drop 4)
  -- let _ = length :: Word32
  c1len <- fromIntegral . toWord32 . BL.take 4 <$> readSTRef src'
  modifySTRef src' (BL.drop 4)
  c1type <- BL.take 4 <$> readSTRef src'
  modifySTRef src' (BL.drop 4)
  tmp <- newSTRef (GLB{version=version, textual=undefined, binary=undefined})
  case BL.unpack c1type of
    [0x4E, 0x4F, 0x53, 0x4A] -> do content <- BL.take c1len <$> readSTRef src'
                                   conv True content tmp
    [0x00, 0x4E, 0x49, 0x42] -> do content <- BL.take c1len <$> readSTRef src'
                                   conv False content tmp
    _                        -> return $ Left "Unknown chunk type at chunk 1"
  modifySTRef src' (BL.drop c1len)
  c2len <- fromIntegral . toWord32 . BL.take 4 <$> readSTRef src'
  modifySTRef src' (BL.drop 4)
  c2type <- BL.take 4 <$> readSTRef src'
  modifySTRef src' (BL.drop 4)
  case BL.unpack c2type of
    [0x4E, 0x4F, 0x53, 0x4A] -> do content <- BL.take c2len <$> readSTRef src'
                                   conv True content tmp
    [0x00, 0x4E, 0x49, 0x42] -> do content <- BL.take c2len <$> readSTRef src'
                                   conv False content tmp
    _                        -> return $ Left "Unknown chunk type at chunk 1"
  Right <$> readSTRef tmp

conv :: Bool -> BL.ByteString -> STRef s GLB -> ST s (Either String ())
conv True src ref = let j = eitherDecode @GLTF src
                    in case j of
                      (Left e) -> return (Left e)
                      (Right j') -> modifySTRef ref (\x -> x{textual=j'}) >> return (Right ())
conv False src ref = modifySTRef ref (\x -> x{binary=BL.toStrict src}) >> return (Right ())

type ComponentType = Int
-- | representation of Int8
pattern BYTE :: ComponentType
pattern BYTE = 5120
-- | representation of Uint8
pattern UNSIGNED_BYTE :: ComponentType
pattern UNSIGNED_BYTE = 5121
-- | representation of Int16
pattern SHORT :: ComponentType
pattern SHORT = 5122 
-- | representation of Uint16
pattern UNSIGNED_SHORT :: ComponentType
pattern UNSIGNED_SHORT = 5123
-- | representation of Int32
pattern UNSIGNED_INT :: ComponentType
pattern UNSIGNED_INT = 5125
-- | representatin of Float
pattern FLOAT :: ComponentType
pattern FLOAT = 5126


type AccessorType = GLTFString
pattern SCALAR :: AccessorType
pattern SCALAR = "SCALAR"

pattern VEC2 :: AccessorType
pattern VEC2 = "VEC2"

pattern VEC3 :: AccessorType
pattern VEC3 = "VEC3"

pattern VEC4 :: AccessorType
pattern VEC4 = "VEC4"

pattern MAT2 :: AccessorType
pattern MAT2 = "MAT2"

pattern MAT3 :: AccessorType
pattern MAT3 = "MAT3"

pattern MAT4 :: AccessorType
pattern MAT4 = "MAT4"

type CameraType = GLTFString
pattern Perspective :: CameraType
pattern Perspective = "perspective"

pattern Orthographic :: CameraType
pattern Orthographic = "orthographic"

data Camera = Camera
  { orthographic :: Maybe OrthographicParam
  , perspective :: Maybe PerspectiveParam
  , cameraType :: CameraType
  , name :: Maybe GLTFString
  , extensions :: Maybe Object
  , extras  :: Maybe Object
  } deriving (Generic)
instance FromJSON Camera
instance ToJSON Camera

data OrthographicParam = OrthographicParam
  { xmag :: GLTFFloat
  , ymag :: GLTFFloat
  , zfar :: GLTFFloat
  , znear :: GLTFFloat
  , extensions :: Maybe Object
  , extras  :: Maybe Object
  } deriving (Generic)

instance FromJSON OrthographicParam
instance ToJSON OrthographicParam

data PerspectiveParam = PerspectiveParam
  { aspectRatio :: Maybe GLTFFloat
  , yfov :: GLTFFloat
  , zfar :: Maybe GLTFFloat
  , znear :: GLTFFloat
  , extensions :: Maybe Object
  , extras  :: Maybe Object
  } deriving (Generic)

instance FromJSON PerspectiveParam
instance ToJSON PerspectiveParam

data Sparse = Sparse
  { count :: Int
  , indices :: Array
  , values :: Array
  , extensions :: Maybe Object
  , extras :: Maybe Object
  } deriving Generic

instance ToJSON Sparse
instance FromJSON Sparse

data Animation = Animation
  { channels :: [Channel]
  , samples :: [AnimationSampler]
  , name :: Maybe GLTFString
  , extensions :: Maybe Object
  , extras :: Maybe Object
  } deriving (Generic)
instance FromJSON Animation
instance ToJSON Animation


data AnimationSampler = AnimationSampler
  { input :: Int
  , interpolation :: Interpolation -- defaults to LINEAR
  , extensions :: Maybe Object
  , extras :: Maybe Object
  }

instance FromJSON AnimationSampler where
  parseJSON = withObject "sampler" $ \kv -> AnimationSampler
    <$> kv .: "input"
    <*> kv .:? "interpolation" .!= LINEARS
    <*> kv .:? "extensions"
    <*> kv .:? "extras"

instance ToJSON AnimationSampler where
  toJSON AnimationSampler{..} = object kvp
    where
      kvp = ["input" .= input] ++
        catMaybes [ toKVP "interpolation" (if interpolation == LINEARS then Nothing else Just interpolation)
                  , toKVP "extensions" extensions
                  , toKVP "extras" extras
                  ]

-- fixme: naming collision.
type Interpolation = GLTFString
pattern LINEARS :: Interpolation
pattern LINEARS = "LINEAR"
pattern STEPS :: Interpolation
pattern STEPS = "STEP"
pattern CUBICSPLINES :: Interpolation
pattern CUBICSPLINES = "CUBICSPLINE"

data Asset = Asset
  { copyright :: Maybe GLTFString
  , generator :: Maybe GLTFString
  , version :: GLTFString
  , minVersion :: Maybe GLTFString
  , extensions :: Maybe Object
  , extras :: Maybe Object
  } deriving (Generic)
instance FromJSON Asset
instance ToJSON Asset

data Buffer = Buffer
  { uri :: GLTFString
  , byteLength :: Maybe Int
  , name :: Maybe GLTFString
  , extensions :: Maybe Object
  , extras :: Maybe Object
  } deriving (Generic)
instance FromJSON Buffer
instance ToJSON Buffer

data BufferView = BufferView
  { buffer :: Int
  , byteOffset :: Int -- defaults to 0
  , byteLength :: Int
  , byteStride :: Maybe Int
  , target :: Maybe Int
  , name :: Maybe GLTFString
  , extensions :: Maybe Object
  , extras :: Maybe Object
  }
instance FromJSON BufferView where
  parseJSON = withObject "bufferView" $ \bv -> BufferView
    <$> bv .: "buffer"
    <*> bv .:? "byteOffset" .!= 0
    <*> bv .: "byteLength"
    <*> bv .:? "byteStride"
    <*> bv .:? "target"
    <*> bv .:? "name"
    <*> bv .:? "extensions"
    <*> bv .:? "extras"

instance ToJSON BufferView where
  toJSON BufferView{..} = object kvp 
    where 
      kvp = ["buffer" .= buffer, "byteLength" .= byteLength] ++ 
        catMaybes
            [ toKVP "byteStride" (byteStride)
            , toKVP "target"     (target)
            , toKVP "name"       (name)
            , toKVP "extensions" (extensions)
            , toKVP "extras"     (extras)
            , toKVP "byteOffset" (if byteOffset == 0 then Nothing else Just byteOffset)
            ]

data Channel = Channel
  { sampler :: Int
  , target :: Object
  , extensions :: Maybe Object
  , extras :: Maybe Object
  } deriving (Generic)

instance FromJSON Channel
instance ToJSON Channel

data Primitive = Primitive
  { attributes :: Object
  , indices :: Maybe Int
  , material :: Maybe MaterialIndex
  , mode :: Mode
  , targets :: [Object]
  , extensions :: Object
  , extras :: Object
  }
instance FromJSON Primitive where
  parseJSON = withObject "primitive" $ \p -> Primitive
    <$> p .: "attributes"
    <*> p .:? "indices"
    <*> p .:? "material"
    <*> p .:? "mode" .!= TRIANGLES
    <*> p .:? "targets" .!= []
    <*> p .:? "extensions" .!= mempty
    <*> p .:? "extras" .!= mempty
instance ToJSON Primitive where
  toJSON Primitive{..} = object kvp
    where 
      kvp = [ "attributes" .= attributes] ++
            catMaybes [ toKVP "indices" indices
                      , toKVP "material" material
                      , toKVP "mode" (if mode == TRIANGLES then Nothing else Just mode)
                      , toKVP "targets" (fromMonoid targets)
                      , toKVP "extensions" (fromMonoid extensions)
                      , toKVP "extras" (fromMonoid extras)
                      ]

type Mode = Int
pattern POINTS :: Mode
pattern POINTS = 0
pattern LINES :: Mode
pattern LINES = 1
pattern LINE_LOOP :: Mode
pattern LINE_LOOP = 2
pattern LINE_STRIP :: Mode
pattern LINE_STRIP = 3
pattern TRIANGLES :: Mode
pattern TRIANGLES = 4
pattern TRIANGLE_STRIP :: Mode
pattern TRIANGLE_STRIP = 5
pattern TRIANGLE_FAN :: Mode
pattern TRIANGLE_FAN = 6

type Offset = Int
type GLTFString = String
gltfStringNull :: GLTFString -> Bool
gltfStringNull = null
type GLTFFloat = Double
type SkinIndex = Int
type CameraIndex = Int
type MeshIndex = Int
type MaterialIndex = Int
type Matrix = [Int]
type NodeIndex = Int

type MimeType = GLTFString
pattern JPEG :: MimeType
pattern JPEG = "image/jpeg"

pattern PNG :: MimeType
pattern PNG = "image/png"

type AlphaMode = GLTFString
pattern OPAQUE :: AlphaMode
pattern OPAQUE = "OPAQUE"
pattern MASK :: AlphaMode
pattern MASK = "MASK"
pattern BLEND :: AlphaMode
pattern BLEND = "BLEND"

type Color = (GLTFFloat, GLTFFloat, GLTFFloat, GLTFFloat)
type Coordinate = (GLTFFloat, GLTFFloat, GLTFFloat, GLTFFloat)

toKVP :: ToJSON v => T.Text -> Maybe v -> Maybe Pair
toKVP s v = (s .=) <$> v

fromMonoid :: (Eq a, Monoid a) => a -> Maybe a
fromMonoid a = if a == mempty then Nothing else Just a
