In this post, we're going to look at how to manipulate multivariate distributions in the HLearn library.
All of these distributions form monoids, and training them is a monoid homomorphism.

We start, as usual, with our language extensions and imports:

>{-# LANGUAGE DataKinds #-}
>{-# LANGUAGE TypeFamilies #-}
>{-# LANGUAGE TemplateHaskell #-}

>import Control.DeepSeq
>import HLearn.Algebra
>import HLearn.Models.Distributions
>import HLearn.Models.Distributions.Multivariate.Internal.Container
>import HLearn.Models.Distributions.Multivariate.Internal.Unital

Next, we'll create a Character data type.
It will have a record for everything we might want to know about a character.

>data Character = Character
>   { _name      :: String
>   , _species   :: String
>   , _job       :: Job
>   , _isGood    :: Maybe Bool
>   , _age       :: Double -- in years
>   , _height    :: Double -- in feet
>   , _weight    :: Double -- in pounds
>   }
>   deriving (Read,Show,Eq,Ord)
>
>data Job = Manager | Crew | Henchman | Other
>   deriving (Read,Show,Eq,Ord)
>
>instance NFData Character where
>   rnf _ = ()
>
>instance NFData Job where
>   rnf j = seq j ()

Now, in order for our library to be able to interpret the Character type, we will need some "type level lenses."
You don't have to know anything about what this means in order to use the library.
All you have to do is call the template haskell function:

>makeIndex ''Character

-- >instance DepIndex Character TH_Name where
-- >   type (#) Character TH_Name = String

>instance HasDepIndex Character where
>   type DepIndexList Character = '[TH_name,TH_species,TH_job,TH_isGood,TH_age,TH_height,TH_weight ]
>   depIndexList _ = TH_name:::TH_species:::TH_job:::TH_isGood:::TH_age:::TH_height:::TH_weight:::HNil

This function creates a bunch of data types and type classes for us.
For example, the type TH_age was created. 
This type gives us a convenient way to access the ages in our distribution, as we'll see shortly.
If you're curious what's going on under the hood here, then checkout the haddock documentation.

That's all we have to do to define our data types.
Now, we're ready to create a data set and start training.
Here's a small data set of our good friends at Planet Express.

>planetExpress = 
>   [ Character "Philip J. Fry"         "human" Crew     (Just True) 1026   5.8 195
>   , Character "Turanga Leela"         "alien" Crew     (Just True) 43     5.9 170
>   , Character "Professor Farnsworth"  "human" Manager  (Just True) 85     5.5 160
>   , Character "Hermes"                "human" Manager  (Just True) 36     5.3 210
>   , Character "Amy Wong"              "human" Other    (Just True) 21     5.4 140
>   , Character "Zoidberg"              "alien" Other    (Just True) 212    5.8 225
>   , Character "Cubert Farnsworth"     "human" Other    (Just True) 8      4.3 135
>   ]

Now, we're ready to train a distribution from this data.
Here's how we would train a distribution where every element is independent of every other element:

>dist1 = train planetExpress :: Multivariate Character
>  '[ Independent Categorical '[String,String,Job,Maybe Bool]
>   , Independent Normal '[Double,Double,Double]
>   ]
>   Double

>data Str = Str { _x :: String, _y :: String }
>
>makeIndex ''Str
>
>strdata = [ Str "Hello" "World", Str "Goodbye" "World" ]
>
>instance HasDepIndex Str where
>   type DepIndexList Str = '[TH_x,TH_y]
>   depIndexList _ = TH_x:::TH_y:::HNil

    >strdist = train ["":::"":::HNil] :: Container Categorical String (Container Categorical String (Unital Double) Double) Double

>strdist = train strdata :: Multivariate Str
>  '[ Independent Categorical '[String,String]
>   ] Float 

    >data Dbl= Dbl { _a :: Double, _b :: Double }
    >
    >makeIndex ''Dbl
    >
    >dbldata = [ Dbl 0 0, Dbl 1 1, Dbl 2 2 ]
    >
    >dbldist = train dbldata :: Multivariate Dbl
    >  '[ Independent Normal '[Double,Double]
    >   ] Double
