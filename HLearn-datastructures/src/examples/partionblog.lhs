\documentclass{tmr}

\title{An NP-complete approximation monoid for the PARTITION problem}
\author{Mike Izbicki\email{mike@izbicki.me}}

\begin{document}
%\maketitle

I enjoy teaching.  
It's really fun helping my intro to C++ students struggle through pointer arithmetic.  
Ah, the joys of segfaults... (to be young and segfaulting...)

And teaching is even better when it causes you to stumble onto an interesting research question.  
Oddly enough, I found this cool Haskell problem while TAing a class in C++ last quarter.

You see, the professor wanted to assign a group project, and I had to pick how the groups would be assigned.  
The only criteria was that groups should be as fair as possible.  
That is, the best and worst students should be evenly distributed throughout all the groups.

"Aha!" I said, "This sounds like the optimization version of the NP-complete PARTITION problem.  
I will use the students' grades as weights, then partition the class into groups so that the total grade of each group is as close as possible."  

Since the class had almost 100 students, there was no way I was going to solve the problem exactly.  
2^100 is a BIG number!  
So I turned to approximation algorithms.  
There is a classic greedy algorithm for solving PARTITION that runs in time $\Theta(n\log n)$ and provides a 4/3-approximation.

I decided to implement this algorithm in Haskell.
What's more, I decided to express the algorithm as a monoid homormorphism and use my HomTrainer type class.

All the examples we've seen so far using HLearn have been directly related to machine learning somehow.
But the library is flexible enough to work with any homomorphism.
In this post, we will approximate the NP-complete PARTITION problem using a monoid homomorphism.

First the preliminaries.  Let's declare which extensions we're using and import our libraries

>{-# LANGUAGE TypeFamilies #-}
>{-# LANGUAGE DataKinds #-}
>
>import Data.Csv
>import System.IO
>import qualified Data.ByteString.Lazy.Char8  as BS
>import qualified Data.Vector  as V
>
>import HLearn.Algebra
>import HLearn.NPHard.Partition

Now, we can define a student data type.  
Every student has a name, which section they're in, and a grade.  
The grade is their current percent grade in the class, so we'll use a Double type to represent it.

>data Student = Student
>   { name    :: String
>   , section :: Int
>   , grade   :: Double
>   }
>   deriving (Read,Show,Eq,Ord)

Now, we need a way to associate a size with each student.
Mathematically, this means we want a norm over the type of students.
Norms need to satisfy a number of laws to make them compatible 

>instance HasRing Student where
>   type Ring (Student) = Double
>
>instance Norm Student where
>   magnitude = grade

Technically, every Norm should also be an instance of VectorSpace.
This would ensure that our magnitude function behaves in a sane way.
For simplicity's sake, however, VectorSpace is not a superclass of Norm.
This means that we don't have to write a bunch of code that we won't use.

Now we're ready to write our main function.
We begin by loading our data from a CSV file:

>main = do
>   Right allStudents <- fmap (fmap (fmap (\(n,s,g) -> Student n s g) . V.toList) . decode True) $ BS.readFile "examples/students.csv"
>        :: IO (Either String [Student])

Then we divide the students up according to their sections:

>   let sec1 = filter (\s -> 1 == section s) allStudents
>   let sec2 = filter (\s -> 2 == section s) allStudents
>   let sec3 = filter (\s -> 3 == section s) allStudents

And calculate a solution to the Partition problem on each section:

>   let solution1 = train sec1 :: Partition Student 5
>   let solution2 = train sec2 :: Partition Student 5
>   let solution3 = train sec3 :: Partition Student 5

To check how good our solution ended up being, we can look at the total cost of each partition.
We do this by:

>   print $ map (sum . map grade) $ partitions solution1

and get the result that:

[348.0,357.0,325.0,400.0,383.0]

325 is a little far from 400, but overall our partitions are fairly evenly sized.

-- >   print $ map name $ getPartition 3 partition1

Now, what if we wanted to look at the 

>   let solutionAll = solution1 <> solution2 <> solution3

>   print $ fmap name $ getPartition 2 solutionAll

\end{document}
