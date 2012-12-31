import Control.Lens
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8  as BS

main = do
    Right rawdata <- fmap (decode True) $ BS.readFile "nukes.csv" 
        :: IO (Either String (V.Vector (String, String, String, Int, Int)))
        
    let usa    = fmap (\row -> (row^._4,row^._5)) $ V.filter (\row -> (row^._1)=="USA"   ) rawdata 
    let russia = fmap (\row -> (row^._4,row^._5)) $ V.filter (\row -> (row^._1)=="Russia") rawdata 
    let china  = fmap (\row -> (row^._4,row^._5)) $ V.filter (\row -> (row^._1)=="China" ) rawdata 
    let uk     = fmap (\row -> (row^._4,row^._5)) $ V.filter (\row -> (row^._1)=="UK"    ) rawdata 
    let france = fmap (\row -> (row^._4,row^._5)) $ V.filter (\row -> (row^._1)=="France") rawdata 
        
    let bomber = fmap (\row -> (row^._4,row^._5)) $ V.filter (\row -> (row^._2)=="Bomber") rawdata 
        
    print usa
    print russia
    print china
    print uk
    print france
    putStrLn ""
    print bomber
        
    putStrLn "Done!"