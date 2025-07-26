module Alpacc.Test.Lexer
  ( mkLexer,
  )
where

import Alpacc.CFG
import Alpacc.Encode
import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Alpacc.Lexer.FSA
import Alpacc.Types
import Alpacc.Util
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder
import Data.ByteString.Internal
import Data.Either.Extra
import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

-- https://hub.darcs.net/thewakalix/bytestring-fuckery/browse/src/Data/ByteString/Fuckery.hs
bsToWord64 :: ByteString -> Either Text Word64
bsToWord64 (BS fp len) =
  if len == 8
    then
      Right . accursedUnutterablePerformIO $
        withForeignPtr fp $
          peek . castPtr @Word8 @Word64
    else Left "Error: Incorrect byte string length for conversion."

mkLexer :: CFG -> Int -> Either Text ([(ByteString, Maybe ByteString)], [(ByteString, Maybe ByteString)])
mkLexer cfg k = do
  spec <- dfaCharToWord8 <$> cfgToDFALexerSpec cfg
  let ts = Map.keys $ regexMap spec
      encoder = encodeTerminals (T "ignore") $ parsingTerminals ts
  dfa <-
    maybeToEither "test" $
      mapTokens (`terminalLookup` encoder) $
        lexerDFA (0 :: Integer) spec
  let ignore = terminalLookup (T "ignore") encoder
      alpha = alphabet $ fsa dfa
      comb = listProducts k $ Set.toList alpha
      tokens str = (str, tokenize dfa ignore str)
      (valid, invalid) = partition (isJust . snd) $ tokens <$> comb
  pure $ ([], [])

builderLexeme :: (Integral t) => UInt -> Lexeme t -> Builder
builderLexeme token_type (Lexeme t (i, j)) =
  t'
    <> word64BE i
    <> word64BE j
  where
    t' =
      case token_type of
        U8 -> word8 $ fromIntegral t
        U16 -> word16BE $ fromIntegral t
        U32 -> word32BE $ fromIntegral t
        U64 -> word64BE $ fromIntegral t

builderLexemes :: (Integral t) => UInt -> Maybe [Lexeme t] -> Builder
builderLexemes _ Nothing = word8 0
builderLexemes token_type (Just ts) =
  word8 1 <> num_bits <> foldMap (builderLexeme token_type) ts
  where
    num_bits = word64BE $ fromIntegral $ numBits token_type

builderInputOutput ::
  (Integral t) =>
  UInt ->
  ([Word8], Maybe [Lexeme t]) ->
  (Builder, Builder)
builderInputOutput token_type (inp, out) =
  ( byteString $ ByteString.pack inp,
    builderLexemes token_type out
  )

-- pLexeme :: UInt -> ByteString -> Either Text (Maybe [Lexeme t])
-- pLexeme token_type str =
--   pure Nothing
--   where
--     num_bytes = num_bits `div` 8
--     (is_valid_str, str') = ByteString.splitAt 1 str
--     (num_bits_str, str'') = ByteString.splitAt 1 str

-- properties :: [(String, Property)]
-- properties =
--   [("parsePrinted", property)]
