\documentclass{tmr}
%include polycode.fmt

\title{A Monoid for Approximating two NP-Complete Problems}
\author{Mike Izbicki\email{mike@@izbicki.me}}

\begin{document}

\begin{introduction}
As a TA I was confronted with a real life instance of the NP-complete Scheduling problem.
To solve the problem, I turned to the classic LPTF approximation algorithm.
In this article, we'll see that because LPTF is a monoid homomorphism, we can implement it using the HomTrainer  type class.
This gives us parallel and iterative versions of LPTF ``for free.''
What's more, LPTF is a particularly illustrative instance of HomTrainer for three reasons.
First, the monoid operation for Scheduling takes non-constant time.
The HomTrainer type class makes reasoning about this run time straightforward.
Second, the monoid uses lazy semantics to improve performance.  
Finally, it demonstrates the versatility of the HomTrainer to many domains.
\end{introduction}

\section{Discovering The Problem}
I enjoy teaching.  
It's really fun helping my intro to C++ students struggle through pointer arithmetic.  
(It's only fun because \emph{I} am not the one spending hours tracking down the segfaults!)
% Ah, the joys of segfaults... 
%(to be young and segfaulting...)
But teaching is even better when it causes you to stumble onto an interesting research question.  
Oddly enough, I found a cool Haskell problem while TAing a class in C++ last quarter.

You see, the professor wanted to assign a group project, and I had to pick the groups.  
The only criteria was that groups should be as fair as possible.  
That is, the best and worst students should be evenly distributed throughout all the groups.

``Aha!'' I said to myself, ``This sounds like the optimization version of the NP-complete PARTITION problem.''  
I will use the students' grades as weights, then partition the class into groups so that the total grade of each group is as close as possible.
% 
Since the class had almost 100 students, there was no way I was going to solve the problem exactly.  
$2^100$ is a BIG number!  
So I turned to approximation algorithms.  
There is a classic greedy algorithm for solving PARTITION that runs in time $\Theta(n\log n)$ and provides a 4/3-approximation.

I decided to implement this algorithm in Haskell.
What's more, I decided to express the algorithm as a monoid homormorphism and use my HomTrainer type class.

\section {Approximate solutions to the Partition problem}



\section{Developing the Partition Monoid}

First, let's look at the data type.

\begin{verbatim}
data Partition a (n::Nat) = Partition
    { bst  :: !(BST a)
    , sets :: Map.Map Int [a]
    }
    deriving (Read,Show,Eq,Ord)
\end{verbatim}

Notice that BST is declared strict, whereas sets is declared lazy.

\section {Using the Partition Monoid}

First, this document is a literate Haskell file.
In order to run it, you'll need to download the latest HLearn-datastructures library:

\begin{verbatim}
 > cabal install hlearn-datastructures-1.0.0
\end{verbatim}

Our example begins, as always, with some language extensions and imports.

\begin{code}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

import Data.Csv
import System.IO
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Data.Vector  as V

import HLearn.Algebra
import HLearn.NPHard.Partition
\end{code}

Now we're ready to start our problem.  
We begin by defining a student data type.  
Every student has a name, a section their enrolled in, and a grade.  
The grade is their current percent grade in the class, so we'll use a Double type to represent it.

\begin{code}
data Student = Student
   { name    :: String
   , section :: Int
   , grade   :: Double
   }
   deriving (Read,Show,Eq,Ord)
\end{code}

The Partition monoid expects to operate on normed data types.
A "norm" is just a fancy math word for "size."
Technically, there are a number of laws that norms must obey, but we can ignore them for our purposes.
Since we want to partition the students in a way that the groups are relatively fair, we'll make the norm the student's grades.

\begin{code}
instance HasRing Student where
   type Ring (Student) = Double

instance Norm Student where
   magnitude = grade
\end{code}

Now we're ready to write our main function and process the data.
We begin by loading our data from a CSV file

\begin{code}
main = do
   Right allStudents <- fmap (fmap (fmap (\(n,s,g) -> 
        Student n s g) . V.toList) . decode True) 
        $ BS.readFile "students.csv"
            :: IO (Either String [Student])
\end{code}

Then we divide the students up according to their sections:

\begin{code}
   let section1 = filter (\s -> 1 == section s) allStudents
   let section2 = filter (\s -> 2 == section s) allStudents
   let section3 = filter (\s -> 3 == section s) allStudents
\end{code}

And calculate a solution to the Partition problem on each section:

\begin{code}
   let solution1 = train section1 :: Partition Student 5
   let solution2 = train section2 :: Partition Student 5
   let solution3 = train section3 :: Partition Student 5
\end{code}

The type signature for Partition says that we're training on the Student type, and that we want to split the group into 5 partitions.

To check how good our solution ended up being, we can look at the total cost of each partition.
The Partition module provides a function:

\begin{verbatim}
partitions :: Partition a n -> [[a]]
\end{verbatim}

for extracting these partitions from the data type.
We do this by:

\begin{code}
   print $ map (sum . map grade) $ partitions solution1
   print $ map (map name) $ partitions solution1
\end{code}

and get the result that:

\begin{verbatim}
[348.0,357.0,325.0,400.0,383.0]
\end{verbatim}

325 is a little far from 400, but overall our partitions are fairly evenly sized.

\begin{code}
-- >   print $ map name $ getPartition 3 partition1
\end{code}

Now, what if we wanted to look at the 

\begin{code}
   let solutionAll = solution1 <> solution2 <> solution3

   print $ fmap name $ getPartition 2 solutionAll
   return ()
\end{code}

\section{Takeaways}

\end{document}