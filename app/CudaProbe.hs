{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Codegen-time probe: compile and run a tiny CUDA program that reports
-- the local device's compute capability and per-block shared memory budget.
-- Used to auto-detect defaults for --sm-arch and --shared-memory on
-- `alpacc cuda`.  Requires nvcc + an NVIDIA GPU on the codegen host.  When
-- either is missing, callers get a Left with a message and should either
-- error out or fall back to explicit values.
module CudaProbe
  ( ProbeResult (..),
    probeDevice,
    ProbeError (..),
  )
where

import Control.Exception (IOException, try)
import Data.List (find, isPrefixOf, stripPrefix)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

data ProbeResult = ProbeResult
  { probeSmArch :: !Int,
    -- ^ Compute capability × 10 (e.g. 75 for sm_75, 80 for sm_80).
    probeSharedMemory :: !Int
    -- ^ Per-block shared memory budget in bytes (cudaDeviceProp::sharedMemPerBlock).
  }
  deriving (Show, Eq)

data ProbeError
  = ProbeNvccMissing !String
  | ProbeCompileFailed !String
  | ProbeRunFailed !String
  | ProbeParseFailed !String
  deriving (Show)

-- | Source of the probe program.  Prints two key=value lines on stdout.
probeSource :: String
probeSource =
  unlines
    [ "#include <cstdio>",
      "#include <cuda_runtime.h>",
      "int main() {",
      "  int dev = 0;",
      "  cudaError_t err = cudaGetDevice(&dev);",
      "  if (err != cudaSuccess) {",
      "    fprintf(stderr, \"cudaGetDevice: %s\\n\", cudaGetErrorString(err));",
      "    return 2;",
      "  }",
      "  cudaDeviceProp p;",
      "  err = cudaGetDeviceProperties(&p, dev);",
      "  if (err != cudaSuccess) {",
      "    fprintf(stderr, \"cudaGetDeviceProperties: %s\\n\", cudaGetErrorString(err));",
      "    return 3;",
      "  }",
      "  int sm = p.major * 10 + p.minor;",
      "  printf(\"sm_arch=%d\\n\", sm);",
      "  printf(\"shared_memory=%zu\\n\", (size_t)p.sharedMemPerBlock);",
      "  return 0;",
      "}"
    ]

-- | Compile + run the probe.  Returns Right on success, Left with a
-- categorised error otherwise.  Callers decide whether to abort or degrade.
probeDevice :: IO (Either ProbeError ProbeResult)
probeDevice = do
  eresult <- try $ withSystemTempDirectory "alpacc-cuda-probe" $ \tmp -> do
    let srcPath = tmp <> "/probe.cu"
        binPath = tmp <> "/probe"
    writeFile srcPath probeSource
    (ec, stdoutS, stderrS) <-
      readProcessWithExitCode
        "nvcc"
        ["-O0", "-std=c++17", "-arch=native", "-o", binPath, srcPath]
        ""
    case ec of
      ExitFailure _ ->
        pure $ Left $ ProbeCompileFailed $ "nvcc failed:\n" <> stderrS <> stdoutS
      ExitSuccess -> do
        (rec, rout, rerr) <- readProcessWithExitCode binPath [] ""
        case rec of
          ExitFailure _ ->
            pure $ Left $ ProbeRunFailed $
              "probe binary failed:\n" <> rerr <> rout
          ExitSuccess ->
            case parseProbeOutput rout of
              Just r  -> pure $ Right r
              Nothing -> pure $ Left $ ProbeParseFailed $
                "could not parse probe stdout:\n" <> rout
  case eresult of
    Left (e :: IOException) ->
      pure $ Left $ ProbeNvccMissing $
        "failed to invoke nvcc: " <> show e
    Right r -> pure r

-- | Parse the two key=value lines the probe prints.  Both required.
parseProbeOutput :: String -> Maybe ProbeResult
parseProbeOutput s = do
  let ls = lines s
      sm = extractInt "sm_arch=" ls
      sh = extractInt "shared_memory=" ls
  ProbeResult <$> sm <*> sh
  where
    extractInt prefix xs =
      case find (prefix `isPrefixOf`) xs of
        Just line -> readMaybe =<< stripPrefix prefix line
        Nothing -> Nothing
