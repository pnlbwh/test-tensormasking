module Mask
  (
    diceCoefficient
  ) where

import qualified Nrrd (diceCoefficient)
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Command
import Control.Monad (when)

diceCoefficient :: FilePath -> FilePath -> Action Float
diceCoefficient mask1 mask2 = withTempDir $ \tmpdir -> do
  let tmpmask1 = tmpdir </> "mask1.nrrd"
  let tmpmask2 = tmpdir </> "mask2.nrrd"
  unit $ cmd "ConvertBetweenFileFormats" mask1 tmpmask1
  unit $ cmd "ConvertBetweenFileFormats" mask1 tmpmask2
  Nrrd.diceCoefficient tmpmask1 tmpmask2