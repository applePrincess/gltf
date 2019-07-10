{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.GLTF.Camera
 ( Camera(..)
 , PerspectiveCamera(..)
 , OrthographicCamera(..)
 ) where

  import GHC.Generics
  import GHC.TypeLits

  import Data.Aeson
  import Data.Aeson.Types (Parser)
  import Data.Scientific (Scientific)

  import Graphics.GLTF.Type
  import Graphics.GLTF.Validation
  
  -- ^ A camera's projection.  A node can reference a camera to apply a transform to place the camera in the scene.
  data Camera = PCamera (Maybe Name) (Maybe Extension) (Maybe Extras) PerspectiveCamera -- ^ An orthographic camera containing properties to create an orthographic projection matrix.
              | OCamera (Maybe Name) (Maybe Extension) (Maybe Extras) OrthographicCamera -- ^ A perspective camera containing properties to create a perspective projection matrix.
              deriving (Generic, Show)
  instance FromJSON Camera where
    parseJSON = withObject "Camera" $ \obj -> do
      t <- obj .: "type" :: Parser String
      
      case t of
        "perspective" -> PCamera <$> obj .:? "name" <*> obj .:? "extensions" <*> obj .:? "extras" <*> (obj .: "perspective")
        "orthographic" -> OCamera <$> obj .:? "name" <*> obj .:? "extensions" <*> obj .:? "extras" <*> (obj .: "orthographic")

  -- |A perspective camera containing properties to create a perspective projection matrix. 
  data PerspectiveCamera = PerspectiveCamera
    { aspectRatio :: Maybe PFloat -- ^ The floating-point aspect ratio of the field of view.
    , yfov :: PFloat -- ^ The floating-point vertical field of view in radians.
    , zfar :: Maybe PFloat -- ^ The floating-point distance to the far clipping plane.
    , znear :: PFloat -- ^ The floating-point distance to the near clipping plane.
    , extensions :: Maybe Extension
    , extras :: Maybe Extras
    } deriving (Generic, Show)
  instance FromJSON PerspectiveCamera where

  -- | An orthographic camera containing properties to create an orthographic projection matrix.
  data OrthographicCamera = OrthographicCamera
    { xmag :: Scientific -- ^ The floating-point horizontal magnification of the view. Must not be zero.
    , ymag :: Scientific -- ^ The floating-point vertical magnification of the view. Must not be zero.
    , zfar :: PFloat -- ^ The floating-point distance to the far clipping plane. `zfar` must be greater than `znear`
    , znear :: NNFloat -- ^ The floating-point distance to the near clipping plane.
    , extensions :: Maybe Extension
    , extras :: Maybe Extras
    } deriving (Generic, Show)
  instance FromJSON OrthographicCamera where
