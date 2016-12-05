{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.HCP.Preprocessing
  (
   AcqParams (..)
  , Index (..)
  , Series (..)
  , B0s (..)
  , rules
  )
  where

import           FSL
import qualified Paths                  (hcpdir)
import           Pipeline.HCP.B0sPair
import qualified Pipeline.HCP.Normalize as N
import           Pipeline.HCP.Types
import           Pipeline.HCP.Util      (posOrientation, readoutTime)
import           Pipeline.Util          (showKey)
import           Shake.BuildNode
import           Text.Printf

stage = "1_Preprocessing"

data AcqParams = AcqParams [Int] CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode AcqParams where
  path k@(AcqParams _ caseid) = Paths.hcpdir caseid stage </> showKey k <.> "txt"
  build k@(AcqParams indices caseid) = Just $ do
    Just phaseEncoding <- fmap read <$> getConfig "phaseEncoding"
    Just echoSpacing <- fmap read <$> getConfig "echoSpacing"
    let dwi0 = N.Dwi (N.DwiScan Pos (head indices), caseid)
    need dwi0
    b0spairs <- N.getB0sPairs caseid indices
    phaseLength <- case posOrientation phaseEncoding of
                     PA -> read . fromStdout
                            <$> command [] "fslval" [nifti dwi0, "dim1"]
                     RL -> read . fromStdout
                            <$> command [] "fslval" [nifti dwi0, "dim2"]
    let readout = printf "%.6f" $ readoutTime phaseLength echoSpacing
        numB0sToUse = length . concatMap _b0indicesToUse
        acqParamsPos =  case posOrientation phaseEncoding of
              PA -> "0 1 0 " ++ readout
              RL -> "1 0 0 " ++ readout
        acqParamsNeg = case posOrientation phaseEncoding of
              PA -> "0 -1 0 " ++ readout
              RL -> "-1 0 0 " ++ readout
        acq = replicate (numB0sToUse $ map _pos b0spairs) acqParamsPos
        acq' = replicate (numB0sToUse $ map _neg b0spairs) acqParamsNeg
    liftIO $ writeFile (path k) $ unlines (acq ++ acq')


--------------------------------------------------------------------------------
-- Index

data Index = Index [Int] CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode Index where
  path k@(Index _ caseid) = Paths.hcpdir caseid stage </> showKey k <.> "txt"
  build k@(Index indices caseid) = Just $ do
    b0spairs <- N.getB0sPairs caseid indices
    liftIO $ writeFile (path k) (unlines $ map show $ mkIndexList b0spairs)

mkIndexList :: [B0sPair] -> [Int]
mkIndexList b0spairs = mkIndex' $ addLast b0indices size
  where
    posSizes = map (_size . _pos) b0spairs
    negSizes = map (_size . _neg) b0spairs
    sizes = scanl (+) 0 $ posSizes ++ negSizes
    size = head . reverse $ sizes
    posb0indices = map (_b0indicesToUse . _pos) b0spairs
    negb0indices = map (_b0indicesToUse . _neg) b0spairs
    b0indices = concat $ zipWith (\is sz -> map (+sz) is) (posb0indices++negb0indices) sizes
    mkIndex' is = reverse $ foldl g [] is
      where g res i =
              let dx = i - length res
                  val = case res of
                    [] -> 1
                    _ -> 1 + head res
              in (replicate dx val) ++ res

addLast :: [a] -> a -> [a]
addLast xs y = reverse . (y:) . reverse $ xs

--------------------------------------------------------------------------------
-- Series

data Series = Series PhaseOrientation [Int] CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode Series where
  path k@(Series orientation _ caseid) = Paths.hcpdir caseid stage
                                      </> showKey k <.> "txt"
  build k@(Series orientation indices caseid) = Just $ do
    ps <- N.getB0sPairs caseid indices
    let
      minsizes = zipWith min (map (_size._pos) $ ps) (map (_size._neg) $ ps)
      series = zipWith printline minsizes $ map (_size. posneg) ps
        where posneg = case orientation of
                Pos -> _pos
                Neg -> _neg
      printline x y = printf "%d %d" x y
    liftIO $ writeFile (path k) $ unlines series


--------------------------------------------------------------------------------
-- B0s

data B0s = B0s PhaseOrientation [Int] CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode B0s where
  path k@(B0s orientation _ caseid) = Paths.hcpdir caseid stage
                                   </> showKey k <.> "nii.gz"
  build k@(B0s orientation indices caseid) = Just $ do
    b0spairs <- N.getB0sPairs caseid indices
    let dwis = [N.Dwi (N.DwiNormalized orientation indices idx, caseid)
               | idx <- indices]
    needs dwis
    let b0indices = map (_b0indicesToUse . posneg) b0spairs
              where posneg = case orientation of
                      Pos -> _pos
                      _   -> _neg
    combineB0s (path k) $ zip (map nifti dwis) b0indices

combineB0s :: FilePath -> [(FilePath, [Int])] -> Action ()
combineB0s out path_and_indices =
  do fs <- traverse (uncurry extractVols) path_and_indices
     mergeVols out fs
     trimVol out


--------------------------------------------------------------------------------
-- Rules

rules = do
  rule (buildNode :: AcqParams -> Maybe (Action [Double]))
  rule (buildNode :: Index -> Maybe (Action [Double]))
  rule (buildNode :: Series -> Maybe (Action [Double]))
  rule (buildNode :: B0s -> Maybe (Action [Double]))
