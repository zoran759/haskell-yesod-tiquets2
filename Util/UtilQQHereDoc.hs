module Util.UtilQQHereDoc(hereDoc) where

import Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote

hereDoc = QuasiQuoter { quoteExp = stringE, quotePat = undefined
                  , quoteType = undefined, quoteDec = undefined }