module Pipeline.HCP.Util
  ( readoutTime
  , posOrientation
  ) where

import Pipeline.HCP.Types

posOrientation :: PhaseEncoding -> PhaseEncoding
posOrientation AP = PA
posOrientation LR = RL
posOrientation x = x

readoutTime :: PhaseLength -> EchoSpacing -> Float
readoutTime l echo = (echo * numPEsteps) / 1000
  where numPEsteps = fromIntegral $ l - 1
