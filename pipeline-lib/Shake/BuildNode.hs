{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Shake.BuildNode
  (module Development.Shake
  ,module Development.Shake.Command
  ,module Development.Shake.FilePath
  ,module Development.Shake.Classes
  ,module Development.Shake.Rule
  ,module Development.Shake.Config
  ,module GHC.Generics
  ,BuildNode (..)
  ,buildNode
  ,need
  ,needs
  ,GithubNode (..)
  ,GitHash
  ,buildGithubNode
  )
  where

import           Control.Monad              (unless, when)
import           Data.Foldable              (traverse_)
import           Data.Time                  (UTCTime (..), utctDayTime)
import           Development.Shake hiding (need)
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

type ShakeKey k  = (Generic k,Typeable k,Show k,Eq k,Hashable k,Binary k,NFData k)

getModTime :: FilePath -> IO Double
getModTime = fmap utcToDouble . getModificationTime
  where
    utcToDouble = fromRational . toRational . utctDayTime

class BuildNode a where
  paths :: a -> [FilePath]
  paths = (:[]) . path

  path :: a -> FilePath
  path = head . paths

  pathDir :: a -> FilePath
  pathDir = takeDirectory . path

  pathPrefix :: a -> FilePath
  pathPrefix = dropExtensions . path

  build :: a -> Maybe (Action ())
  build _ = Nothing

need :: (ShakeKey k, BuildNode k) => k -> Action [Double]
need = apply1

needs :: (ShakeKey k, BuildNode k) => [k] -> Action [[Double]]
needs = apply

instance (ShakeKey k, BuildNode k) => Rule k [Double] where
    storedValue _ q = do
        exists <- traverse IO.doesFileExist $ paths q
        if not (and exists) then return Nothing
        else fmap Just $ traverse getModTime $ paths q
    equalValue _ _ old new = if old == new then EqualCheap else NotEqual

buildNode :: BuildNode a => a -> Maybe (Action [Double])
buildNode k = case (build k) of
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
        exists <- IO.doesFileExist $ cloneDir q </> "clone-success.txt"
        if not exists then return Nothing
        else return $ Just (gitHash q)
    equalValue _ _ old new = if old == new then EqualCheap else NotEqual

buildGithubNode :: GithubNode a => a -> Maybe (Action GitHash)
buildGithubNode k = Just $ do
    let clonedir = cloneDir k
    clonedirExists <- liftIO $ IO.doesDirectoryExist (cloneDir k)
    when clonedirExists $ do
      setWritableRecursive True clonedir
      liftIO $ IO.removeDirectoryRecursive clonedir
    cmd "git clone" (githubUrl k) clonedir :: Action ()
    cmd [Cwd clonedir] "git checkout" (gitHash k) :: Action ()
    case (buildRepo k) of
      Nothing -> return ()
      Just action -> action -- additional actions for particular repo
    writeFile' (clonedir </> "clone-success.txt") (gitHash k)
    setWritableRecursive False clonedir
    return $ gitHash k

-- TODO fails to make read only for everyone
setWritableRecursive :: Bool -> FilePath -> Action ()
setWritableRecursive True root = cmd "chmod -R a+w" root
setWritableRecursive False root = cmd "chmod -R a-w" root
