module Teem
  (mask
  ,fa
  ,gzip
  ,makeMask
  ,isNrrd
  ,center
  ,getB0Indices
  ,extractB0
  ,diceCoefficient
  )
  where

-- Script Deps
-- center.py

import qualified Data.Map                   as M
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           System.Process             (callProcess, readProcess)
import           Teem.Parser                (Result (..), Value (..),
                                             readNrrdHeader)

gzip :: FilePath -> IO ()
gzip out = callProcess "unu" ["save","-e","gzip","-f","nrrd","-i",out,"-o",out]

mask :: FilePath -> FilePath -> FilePath -> IO ()
mask mask vol out = do
  callProcess "unu" ["3op", "ifelse", mask, vol
                    , "0", "-o", out]
  gzip out

fa :: FilePath -> FilePath -> Action ()
fa dwi out = do
  withTempFile $ \tensor -> do
    command_ [] "tend" ["estim","-est","lls","-B","kvp","-knownB0","true","-i",dwi,"-o",tensor]
    command_ [] "tend" ["anvol","-t","-1","-a","fa","-i",tensor,"-o",out]
  liftIO $ gzip out

makeMask :: FilePath -> FilePath -> Action()
makeMask invol outvol = do
      command_ [] "unu" ["3op","ifelse",invol,"1","0","-o",outvol]
      liftIO $ gzip outvol

isNrrd :: FilePath -> Bool
isNrrd file = ext == "nrrd)" || ext == "nhdr"
  where
    ext = takeExtension file

toNifti :: FilePath -> FilePath -> Action ()
toNifti nrrd out = unit $ cmd "ConvertBetweenFileFormats" nrrd out

center :: FilePath -> IO ()
center nrrd = callProcess "center.py"
  ["-i", nrrd
  ,"-o", nrrd]

getB0Indices :: FilePath -> IO [Int]
getB0Indices nrrd = do
  maybeKvps <- Teem.Parser.readNrrdHeader nrrd
  case maybeKvps of
    (Success kvps) -> return
      $ map (read . drop 15 . fst)
      . filter ((== VGradientDirection (0,0,0)) . snd)
      . M.toList
      $ kvps
    failure -> do
      print failure
      error $ "Teem.getB0Indices: Failed to parse nrrd header: " ++ nrrd

extractB0 :: FilePath -> FilePath -> IO ()
extractB0 dwi out = do
  b0index <- head <$> getB0Indices dwi
  callProcess "unu" ["slice"
                    ,"-a", "3"
                    ,"-p", show b0index
                    ,"-i", dwi
                    ,"-o", out]
  gzip out

diceCoefficient :: FilePath -> FilePath -> Action Float
diceCoefficient mask1 mask2 =
  let getVol :: FilePath -> Action Int
      getVol nrrd = do
        Stdout vol <- cmd Shell "unu histo -b 1 -min 1 -max 1 -i" nrrd "| unu save -f text"
        return $ read vol
      intersectVol :: FilePath -> FilePath -> Action Int
      intersectVol nrrd1 nrrd2 = do
        Stdout vol <- cmd Shell "unu 2op x" nrrd1 nrrd2 "| unu histo -b 1 -min 1 -max 1 | unu save -f text"
        return $ read vol
  in do
    vol1 <- fromIntegral <$> getVol mask1
    vol2 <- fromIntegral <$> getVol mask2
    volIntersect <- fromIntegral <$> intersectVol mask1 mask2
    return $ (2 * volIntersect / (vol1 + vol2)) 