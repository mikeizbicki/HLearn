import qualified Data.Map as Map
import qualified Data.Set as S
-- import qualified Data.Vector as V

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST
import Data.Array.ST
import Data.Binary
import Data.Number.LogFloat hiding (log)
import Debug.Trace
import GHC.Arr
import System.Random
import System.IO.Unsafe

main=do
    print [1..1000]