{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Pipeline.HCP.Normalize
  ( B0sPairsYaml (..)
  , MeanB0 (..)
  , Dwi (..)
  , DwiType (..)
  , getB0sPairs
  , rules
  )
  where

import Data.Maybe (fromMaybe)
import           Data.List (findIndices)
import           Data.List.Split      (splitOn)
import           Data.Maybe           (fromJust)
import           Data.Yaml            (decodeFile, encodeFile)
import           FSL                  (BValue (..), FslDwi (..), extractVols_,
                                       mergeVols, readbval, takeBaseName',
                                       tobval, tobvec, writebval, writebvec)
import           Pipeline.HCP.B0sPair (B0sPair (..), mkB0sPair)
import           Pipeline.HCP.Types
import           Pipeline.HCP.Util
import           Pipeline.Util        (showKey)
import           Shake.BuildNode
import qualified System.Directory     as IO (copyFile)
import qualified Paths                (dwiHcp, hcpdir)

stage = "0_Normalize"

newtype MeanB0 = MeanB0 ([Int], CaseId)
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData)

instance BuildNode MeanB0 where
  path (MeanB0 (indices, caseid)) =  Paths.hcpdir caseid stage </> "meanB0.txt"
  build n@(MeanB0 (indices, caseid)) = Just $ do
    let pos0 = Dwi (DwiScan Pos (head indices), caseid)
    need pos0
    mean0 <- getB0sMean (nifti pos0) (bval pos0)
    liftIO $ writeFile (path n) $ show mean0

getB0sMean :: FilePath -> FilePath -> Action Float
getB0sMean dwi bval = do
  Just b0maxbval <- fmap (BValue . read) <$> getConfig "b0MaxBVal"
  b0indices <- findIndices (< b0maxbval) <$> readbval bval
  withTempFile $ \b0s -> do
    extractVols_ b0s dwi b0indices
    command_ [] "fslmaths" [b0s, "-Tmean", b0s]
    Stdout mean <- command [] "fslmeants" ["-i", b0s]
    return $ read mean


--------------------------------------------------------------------------------
-- B0sPairsYaml

newtype B0sPairsYaml = B0sPairsYaml ([Int], CaseId)
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData)

instance BuildNode B0sPairsYaml where
  path n@(B0sPairsYaml (_, caseid)) = Paths.hcpdir caseid stage </> showKey n
  build n@(B0sPairsYaml (indices, caseid)) = Just $ do
    let posdwis = [Dwi (DwiScan Pos idx, caseid) | idx <- indices]
        negdwis = [Dwi (DwiScan Neg idx, caseid) | idx <- indices]
    needs $ posdwis ++ negdwis
    Just b0MaxBVal <- fmap (BValue . read) <$> getConfig "b0MaxBVal"
    Just b0Dist <- fmap read <$> getConfig "b0Dist"
    posbvals <- traverse readBVals posdwis
    negbvals <- traverse readBVals negdwis
    liftIO $ encodeFile (path n) $
      zipWith (mkB0sPair b0MaxBVal b0Dist) posbvals negbvals

getB0sPairs :: CaseId -> [Int] -> Action [B0sPair]
getB0sPairs caseid indices = do
    let n = B0sPairsYaml (indices, caseid)
    need n
    liftIO $ fmap fromJust . decodeFile . path $ n


--------------------------------------------------------------------------------
-- Dwi

data DwiType = DwiScan PhaseOrientation Int
             | DwiNormalized PhaseOrientation [Int] Int
             | DwiJoined PhaseOrientation [Int]
             | DwiJoinedAll [Int]
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

newtype Dwi = Dwi (DwiType, CaseId)
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance FslDwi Dwi where
  nifti (Dwi (DwiScan orientation num, caseid))
    = fromMaybe (error "Set 'dwiHcp path in Paths.hs") $
      Paths.dwiHcp orientation num caseid
  nifti n@(Dwi (_, caseid)) = Paths.hcpdir caseid stage </> showKey n <.> "nii.gz"

instance BuildNode Dwi where
  paths dwi = [nifti dwi, bval dwi, bvec dwi]

  build (Dwi (DwiScan _ _, _)) = Nothing

  build n@(Dwi (DwiNormalized orientation indices idx, caseid)) = Just $ do
    let src = (Dwi (DwiScan orientation idx, caseid))
    need src
    need $ MeanB0 (indices, caseid)
    mean0 <- liftIO $ fmap read $ readFile . path $ MeanB0 (indices, caseid)
    scaleDWI (nifti n) (nifti src) (bval src) mean0
    liftIO $ IO.copyFile (bval src) (bval n)
    liftIO $ IO.copyFile (bvec src) (bvec n)

  build n@(Dwi (DwiJoined orient indices, caseid)) = Just $ do
    let scans = [Dwi (DwiNormalized orient indices idx, caseid) | idx <- indices]
    needs scans
    joinDwis n scans

  build n@(Dwi (DwiJoinedAll indices, caseid)) = Just $ do
    let scans = [Dwi (DwiNormalized orient indices idx, caseid)
                | idx <- indices
                , orient <- [Pos, Neg]]
    needs scans
    joinDwis n scans

joinDwis out dwis = do
        vectors <- fmap concat $ traverse readBVecs dwis
        bvalues <- fmap concat $ traverse readBVals dwis
        writebvec (bvec out) vectors
        writebval (bval out) bvalues
        mergeVols (nifti out) (map nifti dwis)

scaleDWI :: FilePath -> FilePath -> FilePath -> Float -> Action ()
scaleDWI out src srcBval mean0 = do
  mean <- getB0sMean src srcBval
  command_ [] "fslmaths" [src ,"-mul", show mean0 ,"-div", show mean ,out]


rules :: Rules ()
rules = do
    rule (buildNode :: B0sPairsYaml -> Maybe (Action [Double]))
    rule (buildNode :: MeanB0 -> Maybe (Action [Double]))
    rule (buildNode :: Dwi -> Maybe (Action [Double]))
