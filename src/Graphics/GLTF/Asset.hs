{-# LANGUAGE DeriveGeneric #-}
module Graphics.GLTF.Asset 
  (Asset(..)
  ) where
  import GHC.Generics

  import Data.Aeson

  -- Metadata about the glTF asset.
  data Asset = Asset
    { copyright :: Maybe String -- ^ A copyright message suitable for display to credit the content creator.
    , generator :: Maybe String -- ^ Tool that generated this glTF model.  Useful for debugging.
    , version :: String -- ^ The glTF version that this asset targets.
    , minVersion :: Maybe String -- ^ The minimum glTF version that this asset targets.
    } deriving (Generic, Show)
  instance FromJSON Asset where
