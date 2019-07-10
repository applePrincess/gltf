{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Graphics.GLTF where

  import Data.Bifunctor
  import Data.List (unfoldr)
  import Data.List.NonEmpty (NonEmpty)
  import Data.Word
  import GHC.Generics

  import Data.Aeson
  import Data.Aeson.Types (Parser)
  import qualified Data.ByteString.Lazy as BL
  import Network.ByteOrder (word32)

  import Graphics.GLTF.Accessor
  import Graphics.GLTF.Animation hiding (Sampler(..))
  import Graphics.GLTF.Asset
  import Graphics.GLTF.Buffer
  import Graphics.GLTF.BufferView
  import Graphics.GLTF.Camera
  import Graphics.GLTF.Image
  import Graphics.GLTF.Material
  import Graphics.GLTF.Mesh
  import Graphics.GLTF.Node
  import Graphics.GLTF.Sampler
  import Graphics.GLTF.Scene
  import Graphics.GLTF.Skin
  import Graphics.GLTF.Texture
  import Graphics.GLTF.Type
  import Graphics.GLTF.Validation

  data GLTF = GLTF
    { extensionsUsed :: Maybe [String]
    , extensionsRequired :: Maybe [String]
    , accessors :: Maybe [Accessor]
    , animations :: Maybe [Animation]
    , asset :: Asset
    , buffers :: Maybe (NonEmpty Buffer)
    , bufferVeiws :: Maybe (NonEmpty BufferView)
    , cameras :: Maybe (NonEmpty Camera)
    , images :: Maybe (NonEmpty Image)
    , materials :: Maybe (NonEmpty Material)
    , meshes :: Maybe (NonEmpty Mesh)
    , nodes :: Maybe (NonEmpty Node)
    , samplers :: Maybe (NonEmpty Sampler)
    , scene :: Maybe GLTFID
    , scenes :: Maybe (NonEmpty Scene)
    , skins :: Maybe (NonEmpty Skin)
    , textures :: Maybe (NonEmpty Texture)
    } deriving (Generic, Show)
  instance FromJSON GLTF where
    parseJSON = withObject "GLTF" $ \obj -> GLTF
      <$> (obj .:? "extensionsUsed" >>= validateMaybe uniqueAndatLeast1Elem)
      <*> (obj .:? "extensionsRequired" >>= validateMaybe uniqueAndatLeast1Elem)
      <*> (obj .:? "accessors" >>= validateMaybe uniqueAndatLeast1Elem)
      <*> (obj .:? "animations" >>= validateMaybe uniqueAndatLeast1Elem)
      <*> obj .: "asset"
      <*> obj .:? "buffers"
      <*> obj .:? "bufferViews"
      <*> obj .:? "cameras"
      <*> obj .:? "images"
      <*> obj .:? "materials"
      <*> obj .:? "meshes"
      <*> obj .:? "nodes"
      <*> obj .:? "samplers"
      <*> obj .:? "scene"
      <*> obj .:? "scenes"
      <*> obj .:? "skins"
      <*> obj .:? "textures"
      where uniqueAndatLeast1Elem :: Eq a => [a] -> Parser [a]
            uniqueAndatLeast1Elem = both ((validateLength 1), validateUnique)

  data GLB = GLB
    { gltf :: GLTF
    , version :: Word32
    , dat :: Maybe BL.ByteString
    , ext :: [Chunk] -- chunk datum for extension specific.
    } deriving (Generic, Show)
  
  data Chunk = Chunk
    { len :: Word32
    , chunkType :: Word32
    , content :: BL.ByteString
    } deriving (Generic, Show)
  gltfMagic :: BL.ByteString
  gltfMagic = "glTF"

  jsonType :: Word32
  jsonType = 0x4E4F534A
  binaryType :: Word32
  binaryType = 0x004E4942

  parseGLB :: BL.ByteString -> Either String GLB
  parseGLB str | sig /= gltfMagic = Left "magic does not match"
               | (chunkType h) /= jsonType = Left "chunk 1 can only be JSON chunk"
               | otherwise = GLB <$> h' <*> pure ver <*>  pure mBin <*> pure []
    where (sig, str') = BL.splitAt 4 str
          (ver, str'') = first (byteSwap32 . word32 . BL.toStrict) $ BL.splitAt 4 str'
          (totlen, str2) = first (byteSwap32 . word32 . BL.toStrict) $ BL.splitAt 4  str''
          (h:chunks) = parseChunks str2 -- first header must be present
          h' = eitherDecode' (content h)
          mBin :: Maybe BL.ByteString
          mBin = let bh = head chunks 
                 in if not (null chunks) && chunkType bh == binaryType
                    then Just (content bh)
                    else Nothing

  parseChunk :: BL.ByteString -> (Chunk, BL.ByteString)
  parseChunk bl = (Chunk len ty cont, bl3)
    where (len, bl1) = first (byteSwap32 . word32 . BL.toStrict) $ BL.splitAt 4 bl
          (ty, bl2) = first (byteSwap32 . word32 . BL.toStrict) $ BL.splitAt 4 bl1
          (cont, bl3) = BL.splitAt (fromIntegral len) bl2

  parseChunks :: BL.ByteString -> [Chunk]
  parseChunks = unfoldr func
    where func :: BL.ByteString -> Maybe (Chunk, BL.ByteString)
          func bl = if BL.null bl then Nothing else return $ parseChunk bl