{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Graphics.GLTF.Type where

  import Data.Maybe (fromJust, isNothing)
  import GHC.Generics
  import GHC.TypeLits (KnownNat)
  import Numeric.Natural

  import Data.Aeson
  import Data.Scientific (Scientific)
  import qualified Data.Vector.Sized as VS
  import qualified Data.Vector as V

  -- | An identifier used to point another resource.
  newtype GLTFID = GLTFID Natural deriving newtype (Eq, FromJSON)
                                  deriving stock   (Show, Generic)

  -- | A Glenum
  newtype Type = Type Integer deriving newtype (Eq, FromJSON)
                              deriving stock   (Show, Generic)
  
  newtype SizedVec n a = SizedVec (VS.Vector n a) deriving newtype (Eq)
                                                  deriving stock (Show, Generic)

  instance (KnownNat n, FromJSON a) => FromJSON (SizedVec n a) where
    parseJSON = withArray "SizedVec" $ \ary -> do vec <- V.mapM (parseJSON @a) ary
                                                  let lst = VS.fromList @n @a (V.toList vec)
                                                  if isNothing lst
                                                  then fail $ "Could not get typed, sized vector."
                                                  else return $ SizedVec (fromJust lst)
  
  newtype M1ToP1 = M1ToP1 Scientific deriving newtype (Eq, Num, Fractional)
                                     deriving stock   (Show, Generic)
  instance FromJSON M1ToP1 where
    parseJSON = withScientific "M1ToP1" $ \num -> if num < (-1.0) || num > 1.0
                                                  then fail $ "Expected range [-1.0, 1.0], but got: " ++ show num
                                                  else return $ M1ToP1 num
  type Name = String
  type JSONObject = Object