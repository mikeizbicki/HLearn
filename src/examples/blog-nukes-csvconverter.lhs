import Control.Lens
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8  as BS
import System.IO

main = do
    Right rawdata <- fmap (decode True) $ BS.readFile "nukes.csv" 
       :: IO (Either String (V.Vector (String, String, String, Int, Int)))

    let bigList = concat $ V.toList $ fmap (\(country,nuketype,warhead,yeild,num) -> replicate num (country,nuketype,warhead,yeild)) rawdata
    
    hout <- openFile "nukes-list.csv" WriteMode
    sequence_ $ map (\(country,nuketype,warhead,yeild) -> hPutStrLn hout $ show country++","++show nuketype++","++show warhead++","++show yeild) bigList
    hClose hout
    
    print "done"