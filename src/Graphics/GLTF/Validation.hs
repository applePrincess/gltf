{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.GLTF.Validation where

  import Control.Monad (forM_)
  import Data.Maybe
  import Data.List (nub)
  import GHC.Exts (IsList(..))

  import Data.Aeson.Types

  validateByteLength :: Integer -> Parser Integer
  validateByteLength = validateMin "byteLength" 1 False
  
  validateMin :: (Show a, Num a, FromJSON a, Ord a) => String -> a -> Bool -> a -> Parser a
  validateMin tag minVal isExclusive value = if cmp value minVal
                                             then return value
                                             else fail failMsg
    where cmp = if isExclusive then (<) else (<=)
          failMsg = tag ++ " cannnot be less than " ++ if not isExclusive then " or equal to " else "" ++ show minVal
  
  validateLength :: (IsList a) => Int -> a -> Parser a
  validateLength l xs = if length (toList xs) < l
                        then fail $ "array length must be greater than or equal to" ++ show l
                        else return xs

  inRange :: (Show a, Ord a) => a -> a -> a -> Parser a
  inRange mi ma v = if v < mi || v > ma
                    then fail $ "value must be in range [" ++ show mi ++ ", " ++ show ma ++ "]"
                    else return v

  validateCount n | n < 1     = fail "count must be greater than 1"
                  | otherwise = return n

  validateRange :: (IsList a) => a -> Parser a
  validateRange n | length (toList n) `elem` ([1,2,3,4,9,16]::[Int]) = return n
                  | otherwise                      = fail "length of range must be 1,2,3,4,9 or 16"
  
  validateMaybe :: (a -> Parser a) -> Maybe a -> Parser (Maybe a)
  validateMaybe validator = maybe (return Nothing) (sequence . Just . validator)

  validateUnique :: (IsList a, Eq (Item a)) => a -> Parser a
  validateUnique a = if al == a' 
                     then return a 
                     else fail "not unique."
    where a' = nub al
          al = toList a
  -- fail method? to represent failure
  both :: ((a -> Parser a), (a-> Parser a)) -> a -> Parser a
  both (f, g) v = f v >> g v >> return v

  validateAllElements :: (a -> Parser a) -> [a] -> Parser [a]
  validateAllElements fn lst = do
    _ <- forM_ lst fn
    return lst  