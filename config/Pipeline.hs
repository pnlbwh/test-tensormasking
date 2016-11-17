{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
import           Data.List                (intercalate, intersperse)
import           Development.Shake.Config
import           OutputDirectory          (outdir)
import           Shake.BuildKey
import           qualified System.Directory         as IO
-- import           Software.UKFTractography (UKFTractographyExe (..), rules)
import           AntsPath                 (antsPath, antsSrc)
import qualified Intrust
import qualified FSL (threshold, average)
import qualified Nrrd
import qualified Mask (diceCoefficient)


type CaseId = String

data ImageType = Bse | TensorMask
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)


newtype TrainingMask = TrainingMask CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

toNii nrrd out = unit $ cmd "ConvertBetweenFileFormats" nrrd out

instance BuildKey TrainingMask where
  path (TrainingMask caseid) = outdir </> caseid </> (caseid ++"-tensor-mask")
    <.> "nii.gz"

  build out@(TrainingMask caseid) = Just $ withTempFile $ \tmpfile -> do
    let mask = Intrust.path "dwimask" caseid
        tmpnrrd = tmpfile <.> "nrrd"
    need [mask]
    unit $ cmd "center.py -i" mask "-o" tmpnrrd
    toNii tmpnrrd (path out)

newtype TrainingBse = TrainingBse CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey TrainingBse where
  path (TrainingBse caseid) = outdir </> caseid </> (caseid ++ "-bse") <.> "nii.gz"

  build out@(TrainingBse caseid) = Just $ withTempFile $ \tmpf -> do
    let tmpnrrd = tmpf <.> "nrrd"
    let dwied = Intrust.path "dwied" caseid
    need [dwied, replaceExtension dwied "raw.gz"]
    unit $ cmd "bse.sh -i" dwied "-o" tmpnrrd
    unit $ cmd "center.py -i" tmpnrrd "-o" tmpnrrd
    toNii tmpnrrd (path out)

--------------------------------------------------------------------------------
-- AtlasPair

data ThresholdedMasks = ThresholdedMasks | UnThresholdedMasks
                 deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

data AtlasPair = AtlasPair ThresholdedMasks CaseId CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey AtlasPair where
  paths x = [atlasPath Bse x, atlasPath TensorMask x]
    where
      atlasPath imgtype (AtlasPair thresholded caseidT caseid)
        = outdir </> caseidT
        </> (intercalate "-" [show imgtype, thresh, caseid, "in", caseidT])
        <.> "nii.gz"
        where
         thresh = case imgtype of
            Bse -> show UnThresholdedMasks
            _ -> show thresholded

  build out@(AtlasPair UnThresholdedMasks caseidT caseid) = Just $ withTempDir $ \tmpdir -> do
    let bse = TrainingBse caseid
        bseT = TrainingBse caseidT
    apply1 (TrainingMask caseid) :: Action [Double]
    apply [bse, bseT] :: Action [[Double]]
    let pre = tmpdir </> "bse_to_target"
        bseWarped = pre ++ "Warped.nii.gz"
    command_ [] (antsSrc </> "Scripts/antsRegistrationSyN.sh") ["-d", "3"
                                                              ,"-f", path bseT
                                                              ,"-m", path bse
                                                              ,"-o", pre
                                                              ,"-n", "16"]
    let xfmRigid = pre ++ "0GenericAffine.mat"
        xfmWarp = pre ++ "1Warp.nii.gz"
        xfm = pre ++ "Transform.nii.gz"
    command_ [] (antsPath </> "ComposeMultiTransform") ["3"
                                                      ,xfm
                                                      ,"-R", path bseT
                                                      ,xfmWarp
                                                      ,xfmRigid]

    let [bseOut, maskOut] = paths out
    command_ [] (antsPath </> "antsApplyTransforms") ["-d", "3"
                                                    ,"-i", path (TrainingMask caseid)
                                                    ,"-o", maskOut
                                                    ,"-r", path bseT
                                                    ,"-t", xfm]
    liftIO $ IO.copyFile bseWarped bseOut


  build out@(AtlasPair ThresholdedMasks caseidT caseid) = Just $ withTempDir $ \tmpdir -> do
    let atlas = AtlasPair UnThresholdedMasks caseidT caseid
    apply1 atlas :: Action [Double]
    FSL.threshold 0.5 (last . paths $ atlas) (last . paths $ out)

getAtlasSet caseidT thresholdedMasks = do
    Just trainingCases <- fmap words <$> getConfig "trainingCases"
    let atlases = [ AtlasPair thresholdedMasks caseidT caseid
                  | caseid <- trainingCases ]
    apply atlases :: Action [[Double]]
    return atlases


--------------------------------------------------------------------------------
-- DwiMask

data MaskingAlgorithm = MABS
                      | NNLSFusion Int
                      deriving (Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance Show MaskingAlgorithm where
    show MABS = "MABS"
    show (NNLSFusion x) = "NNLSFusionR" ++ show x

data DwiMask = DwiMask MaskingAlgorithm ThresholdedMasks CaseId
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildKey DwiMask where
  path x@(DwiMask _ _ caseid) = filter (/='"') $ outdir </> caseid
                                </> (intercalate "-" (words . show $ x)) <.> "nrrd"

  build out@(DwiMask MABS threshType caseid) = Just $ withTempDir $ \tmpdir -> do
    atlases <- getAtlasSet caseid threshType
    let tmpnii = tmpdir </>  "t.nii.gz"
    FSL.average tmpnii (map (last . paths) atlases)
    FSL.threshold 0.5 tmpnii tmpnii
    unit $ cmd "ConvertBetweenFileFormats" tmpnii (path out)

  build out@(DwiMask (NNLSFusion radius) threshType caseid) = Just $
    withTempFile $ \tmpfile -> do
      atlases <- getAtlasSet caseid threshType
      apply1 (TrainingBse caseid) :: Action [Double]
      let pre = tmpfile
          nnlsOut = tmpfile ++ "_ncc.nii.gz"
          bses = map (head . paths) atlases
          masks = map (last . paths)  atlases
      command_ [] "soft/nnlsFusion" $ ["-r", show radius
                                      ,"-F", path (TrainingBse caseid)
                                      ,"-A"] ++ bses ++
                                      ["-S"] ++ masks ++
                                      ["-O", pre]
      unit $ cmd "ConvertBetweenFileFormats" nnlsOut (path out)
      unit $ cmd "center.py -i" (path out) "-o" (path out)


--------------------------------------------------------------------------------
-- Dice Coefficient

data DiceCoeff = DiceCoeff MaskingAlgorithm ThresholdedMasks CaseId
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

showKey key = filter (/='"') $ intercalate "-" (words . show $ key)
showKeyButLast key = reverse . dropWhile (/='-') . reverse $ showKey key

instance BuildKey DiceCoeff where
  path x@(DiceCoeff _ _ caseid) = outdir
                                  </> caseid
                                  </> (showKey x) <.> "txt"

  build out@(DiceCoeff algo thresh caseid) = Just $ do
    let m = DwiMask algo thresh caseid
    let mT = TrainingMask caseid
    apply1 m :: Action [Double]
    apply1 mT :: Action [Double]
    coeff <- Mask.diceCoefficient (path m) (path mT)
    writeFile' (path out) (show coeff)

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
    usingConfigFile "config/config.cfg"

    want [outdir </> "summary.csv"]

    outdir </> "summary.csv" %> \out -> do
      need [outdir </> "dicecoefficients.csv", "config/summarize.R"]
      cmd "config/summarize.R"

    outdir </> "dicecoefficients.csv" %> \out -> do
        need ["config/caselist.txt"]
        caselist <- readFileLines "config/caselist.txt"
        let coeffs = [DiceCoeff algo thresh caseid
                    | algo <- [MABS, NNLSFusion 4, NNLSFusion 8]
                    , thresh <- [ThresholdedMasks, UnThresholdedMasks]
                    , caseid <- caselist]
        apply coeffs :: Action [[Double]]
        let mkrow coeff = do
              value <- readFile' (path coeff)
              return $ concat . intersperse "," . tail $ (words . show $ coeff) ++ [value]
        rows <- traverse mkrow $ coeffs
        writeFileLines out $ ["algo,thresh,case,coeff"] ++ rows

    rule $ (buildKey :: DiceCoeff -> Maybe (Action [Double]))
    rule $ (buildKey :: DwiMask -> Maybe (Action [Double]))
    rule $ (buildKey :: AtlasPair -> Maybe (Action [Double]))
    rule $ (buildKey :: TrainingBse -> Maybe (Action [Double]))
    rule $ (buildKey :: TrainingMask -> Maybe (Action [Double]))
