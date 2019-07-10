{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Graphics.GLTF.Node
   ( Node(..) 
   ) where

  import Control.Monad (when)
  import Data.List.NonEmpty
  import Data.Maybe (fromJust)
  import GHC.Generics

  import Data.Aeson
  import qualified Data.Vector.Sized as VS
  import qualified Data.HashMap.Strict as HM  

  import Graphics.GLTF.Type

  -- | A node in the node hierarchy.  When the node contains `skin`, all `mesh.primitives` must contain `JOINTS_0` and `WEIGHTS_0` attributes.  A node can have either a `matrix` or any combination of `translation`/`rotation`/`scale` (TRS) properties. TRS properties are converted to matrices and postmultiplied in the `T * R * S` order to compose the transformation matrix; first the scale is applied to the vertices, then the rotation, and then the translation. If none are provided, the transform is the identity. When a node is targeted for animation (referenced by an animation.channel.target), only TRS properties may be present; `matrix` will not be present.
  data Node = Node
    { camera :: Maybe GLTFID -- ^ The index of the camera referenced by this node.
    , children :: Maybe (NonEmpty GLTFID) -- ^ The indices of this node's children.
    , skin :: Maybe GLTFID -- ^ The index of the skin referenced by this node.
    , matrix :: SizedVec 16 Double -- ^ A floating-point 4x4 transformation matrix stored in column-major order.
    , mesh :: Maybe GLTFID -- ^ The index of the mesh in this node.
    , rotation :: SizedVec 4 M1ToP1 -- ^ The node's unit quaternion rotation in the order (x, y, z, w), where w is the scalar.
    , scale :: SizedVec 3 Double -- ^ The node's non-uniform scale, given as the scaling factors along the x, y, and z axes.
    , translation :: SizedVec 3 Double -- ^ The node's translation along the x, y, and z axes.
    , weights :: Maybe (NonEmpty Double) -- ^ The weights of the instantiated Morph Target. Number of elements must match number of Morph Targets of used mesh.
    , name :: Maybe Name
    , extensions :: Maybe Extension
    , extras :: Maybe Extras
    } deriving (Eq, Generic, Show)

  instance FromJSON Node where
    parseJSON = withObject "Node" $ \obj -> do
      when (("matrix" `HM.member` obj) && 
            (("translation" `HM.member` obj) || 
             ("scale" `HM.member` obj) || 
             ("rotation" `HM.member` obj))) $ fail "At least one condition does not meet."
      cam <- obj .:? "camera"
      chi <- obj .:? "children"
      ski <- obj .:? "skin"
      mat <- obj .:? "matrix" .!= defaultMatrix
      mes <- obj .:? "mesh"
      rot <- obj .:? "rotation" .!= defaultRotation
      sca <- obj .:? "scale" .!= defaultScale
      tra <- obj .:? "translation" .!= defaultTranslation
      wei <- obj .:? "weights"
      Node cam chi ski mat mes rot sca tra wei <$> obj .:? "name"<*> obj .:? "extensions" <*> obj .:? "extras"
      where defaultMatrix = SizedVec (fromJust $ VS.fromList [ 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0 ])
            defaultRotation = SizedVec (fromJust $ VS.fromList [ 0.0, 0.0, 0.0, 1.0 ])
            defaultScale = SizedVec (fromJust $ VS.fromList [ 1.0, 1.0, 1.0 ])
            defaultTranslation = SizedVec (fromJust $ VS.fromList [ 0.0, 0.0, 0.0 ])