module Pipeline
  ( rules
  , Pipeline.ANTs.ANTs (..)
  , Pipeline.FreeSurfer.FreeSurfer (..)
  , Pipeline.FreeSurferInDwi.FsInDwi (..)
  , Pipeline.DWI.DwiType (..)
  , Pipeline.DWIMask.DwiMaskType (..)
  , Pipeline.Structural.StructuralType (..)
  , Pipeline.StructuralMask.StructuralMaskType (..)
  , Pipeline.TractQuerier.TractQuerier (..)
  , Pipeline.UKFTractography.UKFTractographyExe (..)
  , Pipeline.UKFTractography.UKFTractographyType (..)
  ) where

import qualified Pipeline.ANTs
import qualified Pipeline.FreeSurfer
import qualified Pipeline.FreeSurferInDwi
import qualified Pipeline.DWI
import qualified Pipeline.DWIMask
import qualified Pipeline.Structural
import qualified Pipeline.StructuralMask
import qualified Pipeline.TractQuerier
import qualified Pipeline.UKFTractography
import qualified Pipeline.HCP

rules = do
  Pipeline.ANTs.rules
  Pipeline.FreeSurfer.rules
  Pipeline.FreeSurferInDwi.rules
  Pipeline.DWI.rules
  Pipeline.DWIMask.rules
  Pipeline.Structural.rules
  Pipeline.StructuralMask.rules
  Pipeline.TractQuerier.rules
  Pipeline.UKFTractography.rules
  Pipeline.HCP.rules
