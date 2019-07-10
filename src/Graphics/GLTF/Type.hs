{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Graphics.GLTF.Type
  ( GLTFID(..)
  , Type(..)
  , SizedVec(..)
  , M1ToP1
  , mkM1ToP1
  , ZToP1
  , mkZToP1
  , PFloat
  , mkPFloat
  , NNFloat
  , mkNNFloat
  , JSONObject
  , Name
  , Extension
  , Extras
  , ByteLength
  ) where

  import Data.Maybe (fromJust, isNothing)
  import Data.Word (Word32)
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
  newtype Type = Type Word32 deriving newtype (Eq, FromJSON, Num)
                             deriving stock   (Show, Generic)
  
  newtype SizedVec n a = SizedVec (VS.Vector n a) deriving newtype (Eq)
                                                  deriving stock (Show, Generic)

  instance (KnownNat n, FromJSON a) => FromJSON (SizedVec n a) where
    parseJSON = withArray "SizedVec" $ \ary -> do vec <- V.mapM (parseJSON @a) ary
                                                  let lst = VS.fromList @n @a (V.toList vec)
                                                  if isNothing lst
                                                  then fail $ "Could not get typed, sized vector."
                                                  else return $ SizedVec (fromJust lst)
  
  newtype M1ToP1 = M1ToP1 Scientific deriving newtype (Eq, Num, Fractional, Ord)
                                     deriving stock   (Show, Generic)
  instance FromJSON M1ToP1 where
    parseJSON = withScientific "M1ToP1" $ \num -> if num < (-1.0) || num > 1.0
                                                  then fail $ "Expected range [-1.0, 1.0], but got: " ++ show num
                                                  else return $ M1ToP1 num
  mkM1ToP1 :: Scientific -> Maybe M1ToP1
  mkM1ToP1 a | a < (1.0) || a > 1.0 = Nothing
             | otherwise            = Just $ M1ToP1 a

  newtype PFloat = PFloat Scientific deriving newtype (Eq, Num, Fractional, Ord)
                                       deriving stock   (Show, Generic)
  instance FromJSON PFloat where
    parseJSON = withScientific "PFloat" $ \num -> if num < 0.0
                                                  then fail $ "Expected positive float, but got: " ++ show num
                                                  else return $ PFloat num
  mkPFloat :: Scientific -> Maybe PFloat
  mkPFloat a | a < 0.0 = Nothing
             | otherwise = Just $ PFloat a
  
  newtype NNFloat = NNFloat Scientific deriving newtype (Eq, Num, Fractional, Ord)
                                       deriving stock   (Show, Generic)
  instance FromJSON NNFloat where
    parseJSON = withScientific "NNFloat" $ \num -> if num <= 0.0
                                                   then fail $ "Expected positive float, but got: " ++ show num
                                                   else return $ NNFloat num
  mkNNFloat :: Scientific -> Maybe NNFloat
  mkNNFloat a | a <= 0.0 = Nothing
              | otherwise = Just $ NNFloat a

  newtype ZToP1 = ZToP1 Scientific deriving newtype (Eq, Num, Fractional, Ord)
                                     deriving stock   (Show, Generic)
  instance FromJSON ZToP1 where
    parseJSON = withScientific "ZToP1" $ \num -> if num < (-1.0) || num > 1.0
                                                 then fail $ "Expected range [-1.0, 1.0], but got: " ++ show num
                                                 else return $ ZToP1 num
  mkZToP1 :: Scientific -> Maybe ZToP1
  mkZToP1 a | a < (1.0) || a > 1.0 = Nothing
            | otherwise            = Just $ ZToP1 a

  newtype PosInt = PosInt Natural deriving newtype (Eq, Num, Integral, Ord)
                                  deriving stock   (Show, Generic)
  instance FromJSON PosInt where
    parseJSON = withScientific "PosInt" $ \num -> if num < 1 || (ceiling num) /= (floor num)
                                                  then fail "Expected positive integer: got " ++ show num
                                                  else return $ PosInt (floor num)
  type JSONObject = Object
  -- | The user-defined name of this object.
  type Name = String
  -- | Dictionary object with extension-specific objects.
  type Extension = JSONObject
  -- | Application-specific data.
  type Extras = Value
  type ByteLength = Natural -- fixme!