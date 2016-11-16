module Nrrd
  (mask
  ,fa
  ,gzip
  ,makeMask
  ,diceCoefficient
  )
  where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Command

gzip :: FilePath -> Action ()
gzip out = command_ [] "unu" ["save","-e","gzip","-f","nrrd","-i",out,"-o",out]

mask :: FilePath -> FilePath -> FilePath -> Action ()
mask mask vol out = do
    unit $ cmd "unu 3op ifelse" mask vol "0" "-o" out
    gzip out

fa :: FilePath -> FilePath -> Action ()
fa dwi out = do
  withTempFile $ \tensor -> do
    command_ [] "tend" ["estim","-est","lls","-B","kvp","-knownB0","true","-i",dwi,"-o",tensor]
    command_ [] "tend" ["anvol","-t","-1","-a","fa","-i",tensor,"-o",out]
  gzip out

makeMask :: FilePath -> FilePath -> Action()
makeMask invol outvol = do
      command_ [] "unu" ["3op","ifelse",invol,"1","0","-o",outvol]
      gzip outvol

diceCoefficient :: FilePath -> FilePath -> Action Float
diceCoefficient mask1 mask2 =
  let getVol :: FilePath -> Action Int
      getVol nrrd = do
        Stdout vol <- cmd Shell "unu histo -b 1 -min 1 -max 1 -i" nrrd "| unu save -f text"
        return $ read vol
      intersectVol :: FilePath -> FilePath -> Action Int
      intersectVol nrrd1 nrrd2 = do
        Stdout vol <- cmd Shell "unu 2op x" nrrd1 nrrd2 "| unu histo -b 1 -min 1 -max 1 | unu save -f text"
        return $ read vol
  in do
    vol1 <- fromIntegral <$> getVol mask1
    vol2 <- fromIntegral <$> getVol mask2
    volIntersect <- fromIntegral <$> intersectVol mask1 mask2
    return $ (2 * volIntersect / (vol1 + vol2))
