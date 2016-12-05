{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Pipeline.ANTs
  ( ANTs (..)
  , rules
  , run
  , getAntsPath
  )
  where

import           Data.Foldable            (traverse_)
import           Development.Shake.Config
import qualified Paths                    (antsPrefix)
import           Shake.BuildNode
import qualified System.Directory         as IO (copyFile,
                                                 createDirectoryIfMissing,
                                                 removeDirectoryRecursive)
import           Util                     (buildGitHubCMake)

newtype ANTs = ANTs GitHash
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance BuildNode ANTs where
  paths (ANTs hash) = map (\f -> Paths.antsPrefix ++ "-" ++ hash </> f)
    ["antsRegistrationSyN.sh"
    ,"antsIntroduction.sh"
    ,"ANTS"
    ,"antsRegistration"
    ,"antsApplyTransforms"
    ,"ComposeMultiTransform"
    ,"PrintHeader"
    ,"ResampleImageBySpacing"
    ,"ImageMath"
    ]

  build out@(ANTs hash) = Just $ do
    let tmpclone = Paths.antsPrefix ++ "-" ++ hash ++ "-tmp"
    buildGitHubCMake ["-DBUILD_EXAMPLES:BOOL=OFF"
                              ,"-DBUILD_TESTING=OFF"
                              ,"-DRUN_LONG_TESTS=OFF"
                              ,"-DRUN_SHORT_TESTS=OFF"] "stnava/ANTs" hash tmpclone
    let madepaths = map (tmpclone </>)
          ["Scripts/antsRegistrationSyN.sh"
          ,"Scripts/antsIntroduction.sh"
          ,"_build/bin/ANTS"
          ,"_build/bin/antsRegistration"
          ,"_build/bin/antsApplyTransforms"
          ,"_build/bin/ComposeMultiTransform"
          ,"_build/bin/PrintHeader"
          ,"_build/bin/ResampleImageBySpacing"
          ,"_build/bin/ImageMath"
          ]
    liftIO $ traverse_ (uncurry IO.copyFile) $ zip madepaths (paths out)
    liftIO $ IO.removeDirectoryRecursive tmpclone

rules = rule (buildNode :: ANTs -> Maybe (Action [Double]))

run script opts = do
  antsPath <- getAntsPath
  command_ [AddEnv "ANTSSRC" antsPath , AddEnv "ANTSPATH" antsPath]
    (antsPath </> script) opts

getAntsPath = do
    Just antsNode <- fmap ANTs <$> getConfig "ANTs-hash"
    need antsNode
    return . pathDir $ antsNode
