{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
import           Data.List        (intercalate, intersperse)
import qualified FSL              (average, threshold)
import qualified Intrust
import qualified Mask             (diceCoefficient)
import           Paths            (outdir)
import           Shake.BuildNode
import qualified System.Directory as IO
import qualified Teem
import qualified Development.Shake as Shake
import qualified Pipeline.ANTs as ANTs
import Pipeline.Util (showKey)


type CaseId = String
toNii nrrd out = unit $ cmd "ConvertBetweenFileFormats" nrrd out

newtype B0 = B0 CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode B0 where
  path n@(B0 caseid) = outdir </> caseid </> showKey n <.> "nii.gz"
  build n@(B0 caseid) = Just $ withTempDir $ \tmpdir -> do
    let tmpnrrd = tmpdir </> "b0.nrrd"
    let dwied = Intrust.path "dwied" caseid
    Shake.need [dwied, replaceExtension dwied "raw.gz"]
    unit $ cmd "bse.sh -i" dwied "-o" tmpnrrd
    unit $ cmd "center.py -i" tmpnrrd "-o" tmpnrrd
    toNii tmpnrrd (path n)

newtype DwiMask = DwiMask CaseId
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode DwiMask where
  path n@(DwiMask caseid) = outdir </> caseid </> showKey n <.> "nii.gz"
  build out@(DwiMask caseid) = Just $ withTempDir $ \tmpdir -> do
    let srcmask = Intrust.path "dwimask" caseid
        tmpnrrd = tmpdir </> "mask.nrrd"
    Shake.need [srcmask]
    unit $ cmd "center.py -i" srcmask "-o" tmpnrrd
    toNii tmpnrrd (path out)

newtype Atlas = Atlas (CaseId, CaseId)
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode Atlas where
  paths n@(Atlas (_, caseidF)) = [outdir </> caseidF </> showKey n ++ "-b0" <.> "nii.gz"
                                 ,outdir </> caseidF </> showKey n ++ "-b0mask" <.> "nii.gz"]
  build n@(Atlas k@(caseidM, caseidF)) = Just $ withTempDir $ \tmpdir -> do
    antspath <- ANTs.getAntsPath
    need (DwiMask caseidM)
    needs [B0 caseidM, B0 caseidF]
    let pre = tmpdir </> "bse_to_target"
    command_ [] (antspath </> "antsRegistrationSyN.sh") ["-d", "3"
                                                        ,"-f", path (B0 caseidF)
                                                        ,"-m", path (B0 caseidM)
                                                        ,"-o", pre, "-n", "16"]
    command_ [] (antspath </> "antsApplyTransforms") ["-d", "3"
                                                     ,"-i", path (B0 caseidM)
                                                     ,"-o", head . paths $ n
                                                     ,"-r", path (B0 caseidF)
                                                     ,"-t", pre++"1Warp.nii.gz", pre++"0GenericAffine.mat"]
    command_ [] (antspath </> "antsApplyTransforms") ["-d", "3"
                                                     ,"-i", path (DwiMask caseidM)
                                                     ,"-o", last . paths $ n
                                                     ,"-r", path (B0 caseidF)
                                                     ,"--interpolation", "NearestNeighbor"
                                                     ,"-t", pre++"1Warp.nii.gz", pre++"0GenericAffine.mat"]


getAtlases caseidF = do
    Just trainingCases <- fmap words <$> getConfig "trainingCases"
    let atlases = [ Atlas (caseidM, caseidF) | caseidM <- trainingCases ]
    needs atlases
    return atlases

img :: Atlas -> FilePath
img = head . paths

mask :: Atlas -> FilePath
mask = last . paths

getAtlasPaths caseid = (,)
                        <$> (map img <$> getAtlases caseid)
                        <*> (map mask <$> getAtlases caseid)

data Algorithm = MABS Float
               | NNLSFusion Int
               | ImNeighborhoodCorrelation Int
               | ImMajorityVoting
               | ImStaple
               | ImAverage
               | DiPy
               | Combined Float [Algorithm]
               | AntsJointFusion [String]
               deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

newtype PredictedMask = PredictedMask (Algorithm, CaseId)
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

fslavg out niis = command_ [] "fslmaths" $
       (intersperse "-add" niis) ++ ["-div", show (length niis), out]

mi :: FilePath -> FilePath -> Action Float
mi im1 im2 = do
  Stdout result <- command [] "ImageMath" ["3", "dummy" ,"Mattes", im1, im2]
  return . read $ result

-- mabsFusion out target imgs masks = withTempDir $ \tmpdir -> do
--   mis <- traverse (mi target) imgs
--   let weights = undefined
--   let weightedimgs = map (tmpdir </>) 


instance BuildNode PredictedMask where
  path n@(PredictedMask (_, caseid)) = outdir </> caseid </> showKey n <.> "nii.gz"

  build n@(PredictedMask ((MABS thr), caseid)) = Just $ do
    (_,masks) <- getAtlasPaths caseid
    fslavg (path n) masks
    FSL.threshold thr (path n) (path n)

  build n@(PredictedMask ((NNLSFusion radius), caseid)) = Just $ withTempDir $ \tmpdir -> do
      (b0s, masks) <- getAtlasPaths caseid
      need (B0 caseid)
      let pre = tmpdir </> "nnlsfusion"
      command_ [] "soft/nnlsFusion" $ ["-r", show radius
                                      ,"-F", path (B0 caseid)
                                      ,"-A"] ++ b0s ++
                                      ["-S"] ++ masks ++
                                      ["-O", pre]
      -- unit $ cmd "ConvertBetweenFileFormats" (tmpfile++"_ncc.nii.gz") (path n)
      liftIO $ IO.copyFile (pre++"_ncc.nii.gz") (path n)

  build n@(PredictedMask ((ImNeighborhoodCorrelation radius), caseid)) = Just $ do
      (b0s,masks) <- getAtlasPaths caseid
      need (B0 caseid)
      unit $ cmd "ImageMath" "3" (path n) "CorrelationVoting" (path $ B0 caseid) b0s masks (show radius)

  build n@(PredictedMask (ImMajorityVoting, caseid)) = Just $ do
      (_,masks) <- getAtlasPaths caseid
      unit $ cmd "ImageMath" "3" (path n) "MajorityVoting" masks

  build n@(PredictedMask (ImAverage, caseid)) = Just $ do
      (_,masks) <- getAtlasPaths caseid
      unit $ cmd "ImageMath" "3" (path n) "AverageLabels" masks

  build n@(PredictedMask (DiPy, caseid)) = Just $ do
      need (B0 caseid)
      unit $ cmd "config/dipy-mask.py" (path $ B0 caseid) (path n)

  build n@(PredictedMask (Combined thr algs, caseid)) = Just $ do
      need (B0 caseid)
      let ms = map (\x -> PredictedMask (x, caseid)) algs
      needs ms
      {-unit $ cmd "AverageImages" "3" (path n) "0" (map path ms)-}
      fslavg (path n) (map path ms)
      FSL.threshold thr (path n) (path n)
      {-unit $ cmd "unu" "2op" "gt" (path out) "0.5" "-o" (path out)-}

  build n@(PredictedMask (AntsJointFusion params, caseid)) = Just $ do
      need (B0 caseid)
      (imgs, masks) <- getAtlasPaths caseid
      unit $ cmd "antsJointFusion" "-d" "3" params "-t" (path $ B0 caseid) "-g" imgs "-l" masks "-o" (path n)

--------------------------------------------------------------------------------
-- Dice Coefficient

newtype DiceCoeff = DiceCoeff (Algorithm, CaseId)
             deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode DiceCoeff where
  path x@(DiceCoeff (_, caseid)) = outdir </> caseid </> (showKey x) <.> "txt"

  build n@(DiceCoeff k@(alg, caseid)) = Just $ withTempDir $ \tmpdir -> do
    need $ DwiMask caseid
    need $ PredictedMask k
    -- coeff <- Mask.diceCoefficient (path $ DwiMask caseid) (path $ PredictedMask k)
    let tmpf = tmpdir </> "dice.txt"
    unit $ cmd "ImageMath" "3" tmpf "DiceAndMinDistSum" (path $ DwiMask caseid) (path $ PredictedMask k)
    coeff <- liftIO $ (last . words) <$> readFile tmpf
    liftIO $ writeFile (path n) coeff
    -- liftIO $ writeFile (path n) (show coeff)

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=outdir, shakeVerbosity=Chatty} $ do
    usingConfigFile "config/settings.cfg"

    want [outdir </> "summary.csv"]

    outdir </> "summary.csv" %> \out -> do
      Shake.need [outdir </> "dicecoefficients.csv"
                 ,"config/summarize.R"]
      cmd "config/summarize.R"

    outdir </> "dicecoefficients.csv" %> \out -> do
        Shake.need ["config/caselist.txt"]
        Shake.need ["config/Main.hs"]
        caselist <- readFileLines "config/caselist.txt"
        let coeffs = [DiceCoeff (alg, caseid)
                    | alg <- [ MABS 0.4, MABS 0.5, MABS 0.6, MABS 0.2, MABS 0.9
                              , NNLSFusion 2, NNLSFusion 4, NNLSFusion 8
                              , ImNeighborhoodCorrelation 2
                              , ImNeighborhoodCorrelation 5
                              , ImNeighborhoodCorrelation 8
                              , ImMajorityVoting
                              , DiPy
                              , Combined 0.4 [MABS 0.5, DiPy, NNLSFusion 4]
                              , Combined 0.2 [MABS 0.5, DiPy, NNLSFusion 4]
                              , Combined 0.5 [MABS 0.5, DiPy, NNLSFusion 4, ImNeighborhoodCorrelation 5]
                              , Combined 0.5 [MABS 0.5, DiPy, ImNeighborhoodCorrelation 5]
                              , Combined 0.5 [MABS 0.6, NNLSFusion 2, ImNeighborhoodCorrelation 2]
                              , Combined 0.6 [MABS 0.6, NNLSFusion 2, ImNeighborhoodCorrelation 2]
                              , Combined 0.7 [MABS 0.6, NNLSFusion 2, ImNeighborhoodCorrelation 2]
                              , Combined 0.2 [MABS 0.6, NNLSFusion 2, ImNeighborhoodCorrelation 2]
                              , AntsJointFusion []
                              ]
                    , caseid <- caselist]
        needs coeffs
        let mkrow n@(DiceCoeff (alg, caseid)) = do
              dice <- readFile (path n)
              return $ concat . intersperse "," $ ["\""++show alg++"\"","\""++caseid++"\"",dice]
        rows <- liftIO $ traverse mkrow $ coeffs
        writeFileLines out $ ["alg,case,coeff"] ++ rows

    rule $ (buildNode :: DiceCoeff -> Maybe (Action [Double]))
    rule $ (buildNode :: DwiMask -> Maybe (Action [Double]))
    rule $ (buildNode :: Atlas -> Maybe (Action [Double]))
    rule $ (buildNode :: B0 -> Maybe (Action [Double]))
    rule $ (buildNode :: PredictedMask -> Maybe (Action [Double]))
    ANTs.rules


  -- build n@(PredictedMask ImStaple threshType caseid) = Just $ do
  --     atlases <- getAtlasSet caseid threshType
  --     apply1 (TrainingBse caseid) :: Action [Double]
  --     let  masks = map (last . paths)  atlases
  --          tmpout = (dropExtensions $ path out) ++  "0001.nrrd"
  --     unit $ cmd "ImageMath" "3" (path out) "STAPLE" masks
  --     FSL.threshold 0.5 (path out) (path out)
      -- unit $ cmd "unu" ["2op", "gt", tmpout, "0.5", "-o", path out]
      -- Teem.gzip (path out) -- TODO
      -- liftIO $ IO.removeFile tmpout
