{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Shake.BuildKey
  (module Development.Shake
  ,module Development.Shake.Command
  ,module Development.Shake.FilePath
  ,module Development.Shake.Classes
  ,module Development.Shake.Rule
  ,module GHC.Generics
  ,BuildKey (..)
  ,buildKey
  ,GithubNode (..)
  ,GitHash
  ,buildGithubNode
  )
  where

import           Control.Monad              (unless, when)
import           Data.Foldable              (traverse_)
import           Data.Time                  (UTCTime (..), utctDayTime)
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Command
import           Development.Shake.Config
import           Development.Shake.FilePath
import           Development.Shake.Rule     (EqualCost (..), Rule (..), apply,
                                             apply1, rule)
import           Development.Shake.Util
import           GHC.Generics
import           System.Directory           as IO
import           Text.Printf
import           System.Directory.PathWalk  (pathWalk)

type CaseId = String
type ShakeKey k  = (Generic k,Typeable k,Show k,Eq k,Hashable k,Binary k,NFData k)

getModTime :: FilePath -> IO Double
getModTime = fmap utcToDouble . getModificationTime
  where
    utcToDouble = fromRational . toRational . utctDayTime

class BuildKey a where
  paths :: a -> [FilePath]
  paths = (:[]) . path

  path :: a -> FilePath
  path = head . paths

  pathPrefix :: a -> FilePath
  pathPrefix = dropExtensions . path

  build :: a -> Maybe (Action ())
  build _ = Nothing

instance (ShakeKey k, BuildKey k) => Rule k [Double] where
    storedValue _ q = do
        exists <- traverse IO.doesFileExist $ paths q
        if not (and exists) then return Nothing
        else fmap Just $ traverse getModTime $ paths q
    equalValue _ _ old new = if old == new then EqualCheap else NotEqual

buildKey :: BuildKey a => a -> Maybe (Action [Double])
buildKey k = case (build k) of
  Nothing -> Just $ liftIO $ traverse getModTime $ paths k -- No action, source node
  (Just action) -> Just $ do
      liftIO $ traverse (createDirectoryIfMissing True) $ map takeDirectory . paths $ k
      action
      liftIO $ traverse getModTime $ paths k

type URL = String
type GitHash = String

class GithubNode a where
  cloneDir :: a -> FilePath
  buildRepo :: a -> Maybe (Action ())
  gitHash :: a -> GitHash
  githubAddress :: a -> String

  githubUrl :: a -> URL
  githubUrl a = "https://github.com" </> (githubAddress a)

instance (ShakeKey k, GithubNode k) => Rule k GitHash where
    storedValue _ q = do
        exists <- IO.doesFileExist $ cloneDir q </> "ppl-stamp.txt"
        if not exists then return Nothing
        else return $ Just (gitHash q)
    equalValue _ _ old new = if old == new then EqualCheap else NotEqual

buildGithubNode :: GithubNode a => a -> Maybe (Action GitHash)
buildGithubNode k = Just $ do
    let clonedir = cloneDir k
    cloneExists <- liftIO $ IO.doesDirectoryExist clonedir
    stampExists <- liftIO $ IO.doesFileExist $ clonedir </> "ppl-stamp.txt"
    unless stampExists
      (do
          liftIO $ when cloneExists $ IO.removeDirectoryRecursive clonedir
          cmd "git clone" (githubUrl k) clonedir :: Action ()
      )
    cmd [Cwd clonedir] "git checkout" (gitHash k) :: Action ()
    case (buildRepo k) of
      Nothing -> return ()
      Just action -> action
    pathWalk (cloneDir k) $ \dir subdirs files ->
     let
        makeReadOnly f = liftIO $ do
          p <- IO.getPermissions (dir </> f)
          IO.setPermissions (dir </> f) (p {IO.writable = False})
      in do
        traverse_ makeReadOnly files
        traverse_ makeReadOnly subdirs
        -- write the stamp file at the last possible moment
        writeFile' (dir </> "ppl-stamp.txt") (gitHash k)
        makeReadOnly (dir </> "ppl-stamp.txt")
        makeReadOnly dir
    return $ gitHash k
