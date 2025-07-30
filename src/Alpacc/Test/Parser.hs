module Alpacc.Test.Parser
  ( parserTests,
  )
where

import Alpacc.CFG
import Data.Binary
import Data.ByteString.Internal
import Data.Text (Text)

newtype Output
  = Output
  { result :: Maybe [Word64]
  }
  deriving (Show)

newtype Outputs
  = Outputs
  { results :: [Output]
  }
  deriving (Show)

newtype Input = Input [Word64] deriving (Show)

newtype Inputs = Inputs [Input] deriving (Show)

instance Binary Output where
  put (Output Nothing) =
    put (False :: Bool)
  put (Output (Just prods)) = do
    put (True :: Bool)
    put (fromIntegral $ length prods :: Word64)
    mapM_ put prods

  get = do
    is_valid <- get :: Get Bool
    if is_valid
      then do
        num_prods <- get :: Get Word64
        prods <- mapM (const (get :: Get Word64)) [1 .. num_prods]
        pure $ Output $ Just prods
      else pure $ Output Nothing

instance Binary Input where
  put (Input tokens) = do
    put (fromIntegral $ length tokens :: Word64)
    mapM_ put tokens

  get = do
    i <- get :: Get Word64
    tokens <- mapM (const get) [1 .. i]
    pure $ Input tokens

instance Binary Inputs where
  put (Inputs inps) = do
    put (fromIntegral $ length inps :: Word64)
    mapM_ put inps

  get = do
    i <- get :: Get Word64
    inps <- mapM (const get) [1 .. i]
    pure $ Inputs inps

instance Binary Outputs where
  put (Outputs results) = do
    put (fromIntegral $ length results :: Word64)
    mapM_ put results

  get = do
    i <- get :: Get Word64
    results <- mapM (const get) [1 .. i]
    pure $ Outputs results

parserTests :: CFG -> Int -> Either Text (ByteString, ByteString)
parserTests cfg k = do
  grammar <- cfgToGrammar cfg
  pure
    ( mempty,
      mempty
    )
