module MABS
  ( mabs
  ) where

import qualified ANTs                     (applyTransforms, computeWarp)
import           Control.Monad            (unless)
import           Data.List                (intercalate)
import           Data.List.Split          (splitOn)
import qualified Development.Shake        as Shake (need)
import qualified FSL                      (average, threshold)
import qualified System.Directory         as IO (copyFile)
import           System.IO.Temp           (withSystemTempFile)
import           Util                     (convertImage)
import Development.Shake
import Development.Shake.FilePath ((</>), (<.>), takeBaseName)


mabs :: FilePath -> [[FilePath]] -> FilePath -> FilePath -> Action ()
mabs antsPath trainingPairs t1 out = withTempDir $ \tmpdir -> do
      registeredmasks <- liftIO $ traverse
        (\(trainingVol:trainingMask:_) -> register antsPath
          tmpdir (trainingVol, trainingMask) t1)
        trainingPairs
      let tmpnii = tmpdir </> "tmp.nii.gz"
      FSL.average tmpnii registeredmasks
      FSL.threshold 0.5 tmpnii tmpnii
      liftIO $ Util.convertImage tmpnii out

register :: FilePath -> FilePath -> (FilePath, FilePath) -> FilePath -> IO FilePath
register antsPath outdir (moving, movingMask) fixed
  = let outMask = outdir </> (intercalate "-" ["Mask"
                                              ,takeBaseName movingMask
                                              ,"in"
                                              ,takeBaseName moving]) <.> "nii.gz"
    in withSystemTempFile "warp.nii.gz" $ \tmpwarp _ -> do
      liftIO $ ANTs.computeWarp antsPath moving fixed tmpwarp
      liftIO $ ANTs.applyTransforms antsPath
        "NearestNeighbor" [tmpwarp] movingMask fixed outMask
      return outMask
