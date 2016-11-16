module FSL
    ( FslDwi (..)
     ,BValue (..)
     ,replaceExtension'
     ,takeBaseName'
     ,extractVol_
     ,extractVol
     ,extractVols_
     ,extractVols
     ,mergeVols
     ,trimVol
     ,getDim3
     ,getDim4
     ,readbval
     ,tobval
     ,writebval
     ,tobvec
     ,readbvec
     ,writebvec
    ,snipDwi
    ,insertSuffix
    ,moveDwi
    ,tonii
    ,dwiToNrrd
    ,threshold
    ,average
    ) where

import           Control.Monad
import           Data.List                  (intersperse)
import           Development.Shake
import           Development.Shake.FilePath
import qualified System.Directory           as IO
import           Text.Printf

newtype BValue = BValue Int
  deriving (Eq, Ord)

instance Show BValue where
  show (BValue b) = show b

data Vec = Vec { vx::Double,
                 vy::Double,
                 vz::Double }

instance Show Vec where
  show (Vec v1 v2 v3) = printf "%f %f %f" v1 v2 v3

class FslDwi a where
  nifti :: a -> FilePath

  bvec :: a -> FilePath
  bvec = tobvec . nifti

  bval :: a -> FilePath
  bval = tobval . nifti

  readBVals :: a -> Action [BValue]
  readBVals = readbval . bval

  readBVecs :: a -> Action [Vec]
  readBVecs = readbvec . bvec

trim :: String -> String
trim = unwords . words

takeBaseName' :: FilePath -> String
takeBaseName' = takeBaseName . takeBaseName

replaceExtension' :: FilePath -> String -> FilePath
replaceExtension' f ext = replaceExtension (dropExtension f) ext

dropExtension' :: FilePath -> FilePath
dropExtension' = dropExtension . dropExtension

insertSuffix :: FilePath -> String -> FilePath
insertSuffix f suff = (++".nii.gz") . (++suff) . dropExtension' $ f

trimVol :: FilePath -> Action ()
trimVol dwi = do
  dim3 <- getDim3 dwi
  when (odd dim3) $ trimVol' dwi
  where
    trimVol' d = withTempFile $ \tmpfile -> do
      putNormal "DWI's have odd number of z-slices, remove one to make even"
      copyFile' d tmpfile
      command [] "fslroi" $ [tmpfile,dwi] ++ map show ([0,-1,0,-1,1,-1] :: [Int])

getDim3 :: FilePath -> Action Int
getDim3 = fmap read . fslval "dim3"

getDim4 :: FilePath -> Action Int
getDim4 = fmap read . fslval "dim4"

fslval :: String -> FilePath -> Action String
fslval key dwi = trim . fromStdout <$> command [] "fslval" [dwi, key]

tobval :: FilePath -> FilePath
tobval f = replaceExtension' f "bval"

readbval :: FilePath -> Action [BValue]
readbval f = map (BValue . read) . words <$> readFile' f

writebval :: FilePath -> [BValue] -> Action ()
writebval out arr = writeFile' out (unwords . map show $ arr)

tobvec :: FilePath -> FilePath
tobvec f = replaceExtension' f "bvec"

-- Reads 3 x N
readbvec :: FilePath ->  Action [Vec]
readbvec f = toVecs <$> map toArr <$> readFileLines f
  where
    toVecs [v1,v2,v3] = zipWith3 Vec v1 v2 v3
    toVecs _ = error $ "Seems to be an invalid bvecs file: " ++ f
    toArr = map read . words

writebvec :: FilePath -> [Vec] -> Action ()
writebvec f dirs = writeFile' f $ unlines [vx',vy',vz']
  where vx' = unwords . map (show . vx) $ dirs
        vy' = unwords . map (show . vy) $ dirs
        vz' = unwords . map (show . vz) $ dirs

extractVol_ :: FilePath -> FilePath -> Int -> Action ()
extractVol_ out dwi idx = command [] "fslroi" [dwi, out, show idx, "1"]

extractVol :: FilePath -> Int -> Action FilePath
extractVol dwi idx = out <$ extractVol_ out dwi idx
  where
    out = insertSuffix dwi (printf "-%03d" idx)

extractVols :: FilePath -> [Int] -> Action FilePath
extractVols dwi indices
  = withTempFile $ \tmpfile -> extractVols_ tmpfile dwi indices >> return tmpfile
    -- out = replaceExtension' dwi (printf "%s.nii.gz" (intercalate "-" $ map show idx))

extractVols_ :: FilePath -> FilePath -> [Int] -> Action ()
extractVols_ out dwi indices = do
  vols <- traverse (extractVol dwi) indices
  mergeVols out vols
  liftIO $ traverse IO.removeFile vols
  return ()

mergeVols :: FilePath -> [FilePath] -> Action ()
mergeVols out vols = unit $ command [] "fslmerge" (["-t", out] ++ vols)

snipDwi :: FilePath -> Int -> Action (FilePath, FilePath)
snipDwi dwi idx = do
  dim4 <- getDim4 dwi
  when (idx < 0) $ error " snipDwi: index must be positive."
  when (idx >= dim4) $ error "snipDwi: index must be smaller than DWI's number of diffusion volumes."
  withTempDir $ \tmpdir -> do
    let prefix = tmpdir </> takeBaseName' dwi
        out1 = insertSuffix "-1" dwi
        out2 = insertSuffix "-2" dwi
    command_ [] "fslsplit" [dwi, prefix]
    vols <- liftIO $ fmap (tmpdir </>) <$> getDirectoryFilesIO tmpdir ["*"]
    mergeVols out1 (take idx vols)
    mergeVols out2 (drop idx vols)
    bvals <- readbval (tobval dwi)
    bvecs <- readbvec (tobvec dwi)
    writebval (tobval out1) (take idx bvals)
    writebvec (tobvec out1) (take idx bvecs)
    writebval (tobval out2) (drop idx bvals)
    writebvec (tobvec out2) (drop idx bvecs)
    return (out1, out2)

moveDwi :: FilePath -> FilePath -> Action ()
moveDwi dwi dwi' = liftIO $ do
  IO.renameFile dwi dwi'
  IO.renameFile (tobval dwi) (tobval dwi')
  IO.renameFile (tobvec dwi) (tobvec dwi')

dwiToNrrd :: [String] -> FilePath -> Action FilePath
dwiToNrrd options dwi = do
  let out = replaceExtension' dwi "nrrd"
  command_ [] "DWIConvert" $ ["--conversionMode"
                           ,"FSLToNrrd"
                           ,"--fslNIFTIFile", dwi
                           ,"--inputBValues", tobval dwi
                           ,"--inputBVectors", tobvec dwi
                           ,"-o", out
                           ] ++ options
  return out


tonii :: FilePath -> FilePath
tonii f = replaceExtension f "nii.gz"

threshold :: Float -> FilePath -> FilePath -> Action ()
threshold thresh nii niiOut =
  command_ [] "fslmaths" [nii, "-thr", show thresh, niiOut]

average :: FilePath -> [FilePath] -> Action ()
average out niis =
  command_ [] "fslmaths" $
  (intersperse "-add" niis)
   ++ ["-div", show (length niis)
      ,out
      ,"-odt", "short"]
