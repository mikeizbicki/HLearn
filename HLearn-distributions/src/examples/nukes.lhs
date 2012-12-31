The Bulletin of the Atomic Scientists tracks the nuclear capabilities of countries.  We're going to use their data to demonstrate the power of algebraic statistics, and the HLearn library.  In particular, we'll look at the nuclear arsenals  of the five countries that are "allowed" to have nuclear weapons by the Non-Proliferation Treaty: the USA, UK, France, Russia and China.  (Israel, Pakistan, India, and North Korea also have nuclear weapons, but are not members of the NPT.)

First, let's import our libraries:

>import Control.Lens
>import Data.Csv
>import qualified Data.Vector as V
>import qualified Data.ByteString.Lazy.Char8  as BS

>main = do
>    Right rawdata <- fmap (decode True) $ BS.readFile "nukes.csv" 
>        :: IO (Either String (V.Vector (String, String, String, Int, Int)))
>        
>    let usa    = fmap (\row -> (row^._4,row^._5)) $ V.filter (\row -> (row^._1)=="USA"   ) rawdata
>    let uk     = fmap (\row -> (row^._4,row^._5)) $ V.filter (\row -> (row^._1)=="UK"    ) rawdata 
>    let france = fmap (\row -> (row^._4,row^._5)) $ V.filter (\row -> (row^._1)=="France") rawdata 
>    let russia = fmap (\row -> (row^._4,row^._5)) $ V.filter (\row -> (row^._1)=="Russia") rawdata 
>    let china  = fmap (\row -> (row^._4,row^._5)) $ V.filter (\row -> (row^._1)=="China" ) rawdata 
>        
>    print usa

** Operators, Modules, and the Free Module

If you're familiar with the Data.Sequence package, you'll recognize the operators (<|) and (|>).  That's because Sequence is an operator over the 

** Plotting the distributions

** Survivable nuclear weapons and module subtraction

>    print russia
>    print china
>    print uk
>    print france
>    putStrLn ""
        
>    let bomber = fmap (\row -> (row^._4,row^._5)) $ V.filter (\row -> (row^._2)=="Bomber") rawdata 
>    print bomber
>        
>    putStrLn "Done!"