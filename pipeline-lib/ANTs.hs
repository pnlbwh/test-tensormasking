module ANTs
  (computeRigid
  ,computeWarp
  ,applyTransforms
  ,upsample
  ) where

-- import Software.ANTs (ANTs (..))
import           Data.Maybe         (fromMaybe)
import           System.Directory   as IO (copyFile)
import           System.Environment (lookupEnv)
import           System.FilePath    ((</>))
import           System.IO.Temp     (withSystemTempDirectory,
                                     withSystemTempFile)
import           System.Process     (callProcess)
import System.Directory as IO (doesDirectoryExist)
import Control.Monad.Extra (whenM)


getAnts :: FilePath -> IO FilePath
getAnts "" = do envPath <- fmap (fromMaybe $ error "getAnts: ANTSPATH not set, set it or call function with a path")
                 (lookupEnv "ANTSPATH")
                case envPath of
                  "" -> error "getAnts: ANTSPATH is set to an empty path."
                  _ -> getAnts envPath
getAnts path = do
  whenM (not <$> IO.doesDirectoryExist path)
    (error $ "getAnts: the path " ++ path ++ " does not exist")
  return path


-- TODO replace ANTS with newer antsRegistration
computeRigid antspath moving fixed outtxt
  = withSystemTempDirectory "" $ \tmpdir -> do
    let pre = tmpdir </> "ants"
        affine = pre ++ "Affine.txt"
    callProcess (antspath </> "ANTS") ["3"
                                      ,"-m", "MI["++fixed++","++moving++",1,32]"
                                      ,"-i", "0",
                                       "-o", pre, "--do-rigid"]
    IO.copyFile affine outtxt

computeWarp antspath moving fixed outwarp
  = withSystemTempDirectory "" $ \tmpdir -> do
    let pre = tmpdir </> "ants"
        affine = pre ++ "0GenericAffine.mat"
        warp = pre ++ "1Warp.nii.gz"
    -- antsRegistrationSyN uses MI for the Rigid and Affine stages,
    -- and CC with radius 4 for the non-linear BSplineSyN stage
    callProcess (antspath </> "antsRegistrationSyN.sh")
      ["-d", "3"
      ,"-f", fixed
      ,"-m", moving
      ,"-o", pre
      ,"-n", "16"]
    callProcess (antspath </> "ComposeMultiTransform")
      ["3", outwarp ,"-R", fixed, warp, affine]

applyTransforms antspath interpolation transforms moving fixed out =
  callProcess (antspath </> "antsApplyTransforms") $
    ["-d", "3"
    ,"-i", moving
    ,"-o", out
    ,"-r", fixed
    ,"-t"] ++ transforms
    ++ (if null interpolation then [] else ["--interpolation", interpolation])

upsample antspath spacings img out
  = callProcess (antspath </> "ResampleImageBySpacing")
  ["3", img, out, unwords . map show $ spacings]
