module Alpacc.Test.Parser
  ( parserTests,
  )
where

import Alpacc.CFG
import Alpacc.Encode
import Alpacc.Grammar
import Alpacc.LLP
import Alpacc.Util
import Data.Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal
import Data.Maybe
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

parserTests :: CFG -> Int -> Int -> Int -> Either Text (ByteString, ByteString)
parserTests cfg q k n = do
  grammar <- cfgToGrammar cfg
  let s_encoder = encodeSymbols (T "ignore") grammar
      p x = x /= AugmentedTerminal Unused && x /= LeftTurnstile && x /= RightTurnstile
      encode' x = fromJust $ Terminal x `symbolLookup` s_encoder
      comb =
        listProducts n $
          mapMaybe unaug $
            filter p $
              terminals $
                getGrammar grammar
      inputs = Inputs $ Input . fmap (fromIntegral . encode' . AugmentedTerminal) <$> comb
      parse = llpParse q k (getGrammar grammar)
      outputs = Outputs $ Output . fmap (fmap fromIntegral) . toMaybe . parse <$> comb
  pure
    ( ByteString.toStrict $ encode inputs,
      ByteString.toStrict $ encode outputs
    )
  where
    toMaybe (Left _) = Nothing
    toMaybe (Right a) = Just a
    unaug (AugmentedTerminal t) = Just t
    unaug _ = Nothing
