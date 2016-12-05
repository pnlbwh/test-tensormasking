module Pipeline.HCP
  ( -- Pipeline.HCP.PostEddy.HcpDwi (..)
    rules
  ) where

import qualified Pipeline.HCP.Normalize
import qualified Pipeline.HCP.Preprocessing
import qualified Pipeline.HCP.Topup
import qualified Pipeline.HCP.Eddy
-- import qualified Pipeline.HCP.PostEddy

rules = do
  Pipeline.HCP.Normalize.rules
  Pipeline.HCP.Preprocessing.rules
  Pipeline.HCP.Topup.rules
  Pipeline.HCP.Eddy.rules
  -- Pipeline.HCP.PostEddy.rules
