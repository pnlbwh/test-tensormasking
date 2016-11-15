module SoftwareOutputPaths
  (ukfTractographyExe
  ,tractQuerierDir)
  where

import           Development.Shake.FilePath (FilePath, (</>))
import           OutputDirectory            (outdir)

ukfTractographyExe :: String -> FilePath
ukfTractographyExe hash = outdir </> "software" </> ("UKFTractography-" ++ hash)

tractQuerierDir hash = outdir </> ("tract_querier-" ++ hash)
