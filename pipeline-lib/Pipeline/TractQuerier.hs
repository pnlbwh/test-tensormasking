{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Pipeline.TractQuerier
  (TractQuerier (..)
  ,rules
  )
  where

import           Control.Monad              (unless)
import           Data.Foldable              (traverse_)
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import qualified Paths                      (tractQuerierPrefix)
import           Shake.BuildNode
import qualified System.Directory           as IO
import           System.Directory.PathWalk  (pathWalk)


newtype TractQuerier = TractQuerier GitHash
        deriving (Show,Generic,Typeable,Eq,Hashable,Binary,NFData,Read)

instance GithubNode TractQuerier where
  gitHash (TractQuerier hash) = hash

  githubAddress _ = "demianw/tract_querier.git"

  cloneDir (TractQuerier hash) = Paths.tractQuerierPrefix ++ "-" ++ hash

  buildRepo out@(TractQuerier hash) = Just $ do
    -- saves 70 MB of space
    liftIO $ IO.removeDirectoryRecursive (cloneDir out </> "doc")
    liftIO $ IO.removeDirectoryRecursive (cloneDir out </> ".git")


rules = rule (buildGithubNode :: TractQuerier -> Maybe (Action String))
