{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Pipeline.DWIMask
  ( DwiMaskType (..)
  , DwiMask (..)
  , rules
  ) where

import           Data.Maybe       (fromMaybe)
import qualified Paths
import           Pipeline.DWI     hiding (rules)
import           Pipeline.Util    (showKey)
import           Pipeline.Util    (showKey)
import           Shake.BuildNode
import           System.Directory as IO (renameFile)

type CaseId = String

data DwiMaskType = DwiMaskGiven
                 | DwiMaskHcp
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

newtype DwiMask = DwiMask (DwiMaskType, DwiType, CaseId)
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode DwiMask where
  path (DwiMask (DwiMaskGiven, _, caseid))
    = fromMaybe (error "Set 'dwimask' in Paths.hs") $ Paths.dwimask caseid
  -- path (DwiMask (DwiMaskGiven, DwiXc, caseid))
  --   = (error "Set 'dwimask' in Paths.hs") $ Paths.dwimask caseid
  path n@(DwiMask (DwiMaskHcp, _, caseid))
    = Paths.outdir </> caseid </> showKey n <.> "nii.gz"

  build (DwiMask (DwiMaskGiven, _, _)) = Nothing

  build n@(DwiMask (DwiMaskHcp, dwitype, caseid))
    = Just $ do
    need $ Dwi (dwitype, caseid)
    unit $ command [] "bet" [path $ Dwi (dwitype, caseid)
                            , caseid
                            , "-m"
                            , "-f"
                            , "0.1"]
    liftIO $ IO.renameFile (caseid ++ "_mask.nii.gz") (path n)


rules = rule (buildNode :: DwiMask -> Maybe (Action [Double]))
