module Main where
import DFA (DFA, isEmpty)
--Here we import all the functions and constructors that will be used in the
--tests
import Test.HUnit ( Test, runTestTT, Test(TestCase), errors, failures, test )
import System.Exit (exitSuccess, exitFailure)
import Test.HUnit.Base (assertEqual)
--The automata we will be testing are going to receive strings from the binary
--alphabet. So we create the symbol type based on the binary alphabet.
data Sym = Zero | One deriving (Show,Eq)
data St = Q0|Q1|Q2|Q3 deriving (Show,Eq)
-- Create first DFA and its transition function.
firstDFA :: DFA St Sym
firstDFA = ([Q0, Q1], [Zero, One], t1, Q0, [Q1])
  where
    t1 :: St -> Sym -> St
    t1 Q0 Zero = Q0
    t1 Q0 One = Q0
    t1 Q1 Zero = Q1
    t1 Q1 One = Q1
    t1 _ _ =Q0
-- Create second DFA and its transition function.
secondDFA :: DFA St Sym
secondDFA = ([Q0, Q1, Q2, Q3], [Zero, One], t2, Q0, [Q3])
  where
    t2 :: St -> Sym -> St
    t2 Q0 Zero = Q1
    t2 Q0 One = Q0
    t2 Q1 Zero = Q2
    t2 Q1 One = Q2
    t2 Q2 Zero = Q1
    t2 Q2 One = Q1
    t2 Q3 Zero = Q3
    t2 Q3 One = Q3
-- Create third DFA and its transition function.
thirdDFA :: DFA St Sym
thirdDFA = ([Q0, Q1, Q2], [Zero, One], t3, Q0, [Q1])
  where
    t3 :: St -> Sym -> St
    t3 Q0 Zero = Q2
    t3 Q0 One = Q2
    t3 Q1 Zero = Q2
    t3 Q1 One = Q0
    t3 Q2 Zero = Q1
    t3 Q2 One = Q0
    t3 _ _ = Q2
-- Create fourth DFA and its transition function.
fourthDFA :: DFA St Sym
fourthDFA = ([Q0, Q1], [Zero, One], t4, Q0, [])
  where
    t4 :: St -> Sym -> St
    t4 Q0 Zero = Q1
    t4 Q0 One = Q1
    t4 Q1 Zero = Q0
    t4 Q1 One = Q0
    t4 _ _ =Q0
-- Create fifth DFA and its transition function.
fifthDFA :: DFA St Sym
fifthDFA = ([Q0, Q1,Q2,Q3], [Zero, One], t5, Q0, [Q2])
  where
    t5 :: St -> Sym -> St
    t5 Q0 Zero = Q2
    t5 Q0 One = Q1
    t5 Q1 Zero = Q0
    t5 Q1 One = Q1
    t5 Q2 Zero = Q1
    t5 Q2 One = Q3
    t5 Q3 Zero = Q3
    t5 Q3 One = Q3
-- Create the five test cases for each DFA. Each one of them recieves the
-- function 'assertEqual' which compares the expected value with the actual
-- value produced by the code being tested, it recieves 3 parameters: the first
-- one is a description of the assertion, the second one is the expected 
-- boolean to the question 'Is the language of the DFA empty?', finally, the 
-- fourth and last element is the actual boolean returned by the checking 
-- function.
test1 :: Test
test1 = TestCase (assertEqual "Empty DFA should return True" True (isEmpty firstDFA))

test2 :: Test
test2 = TestCase (assertEqual "Empty DFA should return True" True (isEmpty secondDFA))

test3 :: Test
test3 = TestCase (assertEqual "Empty DFA should return True" False (isEmpty thirdDFA))

test4 :: Test
test4 = TestCase (assertEqual "Empty DFA should return True" True (isEmpty fourthDFA))

test5 :: Test
test5 = TestCase (assertEqual "Empty DFA should return True" False (isEmpty fifthDFA))
--Main function declaration, the IO () type constructor allows the 
--representation of actions as Haskell values.
--The main function is where the tests will be carried out. The variable counts
--is the result of running a series of tests using runTestTT. RunTestsTT
--generates a report that lists the names and results of each test. Finally,
--the errors and failures functions return the number of tests that have
--encountered errors and failures. If the number of errors and failures equals
--0, then all the tests were successful.
--Code taken from:
--https://putridparrot.com/blog/unit-testing-haskel-code-with-hunit/
--Information taken from:
--https://hackage.haskell.org/package/HUnit-1.6.2.0/docs/Test-HUnit-Text.html 
--https://hackage.haskell.org/package/HUnit-1.6.2.0/docs/Test-HUnit-Base.html#g:4
main :: IO ()
main = do
  counts <- runTestTT ( test [
    test1,
    test2,
    test3,
    test4,
    test5
    ])
  if errors counts + failures counts == 0
      then exitSuccess
      else exitFailure