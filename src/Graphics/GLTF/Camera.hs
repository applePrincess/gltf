{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GLTF.Camera where

  import GHC.Generics
  import GHC.TypeLits

  import Data.Aeson
  import Data.Aeson.Types (Parser)

  import Graphics.GLTF.Validation
  
  -- ^ A camera's projection.  A node can reference a camera to apply a transform to place the camera in the scene.
  data Camera = PCamera PerspectiveCamera -- ^ An orthographic camera containing properties to create an orthographic projection matrix.
              | OCamera OrthographicCamera -- ^ A perspective camera containing properties to create a perspective projection matrix.
              deriving (Generic, Show)
  instance FromJSON Camera where
    parseJSON = withObject "Camera" $ \obj -> do
      t <- obj .: "type" :: Parser String
      case t of
        "perspective" -> PCamera <$> (obj .: "perspective")
        "orthographic" -> OCamera <$> (obj .: "orthographic")

  -- |A perspective camera containing properties to create a perspective projection matrix. 
  data PerspectiveCamera = PerspectiveCamera
    { aspectRatio :: Maybe Double -- ^ The floating-point aspect ratio of the field of view.
    , yfov :: Double -- ^ The floating-point vertical field of view in radians.
    , zfar :: Maybe Double -- ^ The floating-point distance to the far clipping plane.
    , znear :: Double -- ^ The floating-point distance to the near clipping plane.
    -- , extensions, extras
    } deriving (Generic, Show)
  instance FromJSON PerspectiveCamera where
    parseJSON = withObject "PerspectiveCamera" $ \obj -> PerspectiveCamera
      <$> (obj .:? "aspectRatio" >>= validateMaybe (validateMin "aspectRatio" 0.0 True))
      <*> (obj .: "yfov" >>= validateMin "yfov" 0.0 True)
      <*> (obj .:? "zfar" >>= validateMaybe (validateMin "zfar" 0.0 True))
      <*> (obj .: "znear" >>= validateMin "znear" 0.0 True)

  -- | An orthographic camera containing properties to create an orthographic projection matrix.
  data OrthographicCamera = OrthographicCamera
    { xmag :: Double -- ^ The floating-point horizontal magnification of the view. Must not be zero.
    , ymag :: Double -- ^ The floating-point vertical magnification of the view. Must not be zero.
    , zfar :: Double -- ^ The floating-point distance to the far clipping plane. `zfar` must be greater than `znear`
    , znear :: Double -- ^ The floating-point distance to the near clipping plane.
    -- , extensions, extras
    } deriving (Generic, Show)
  instance FromJSON OrthographicCamera where
    parseJSON = withObject "OrthographicCamera" $ \obj -> OrthographicCamera
      <$> (obj .: "xmag" >>= validateMin "xmag" 0.0 True)
      <*> (obj .: "ymag" >>= validateMin "ymag" 0.0 True)
      <*> (obj .: "zfar" >>= validateMin "zfar" 0.0 True)
      <*> (obj .: "znear" >>= validateMin "znear" 0.0 True)
         