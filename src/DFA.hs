module DFA where
import Data.List (intersect)
-- Create type DFA.
type DFA st sym =
    ( [st] -- States
    , [sym] -- Input symbols
    , st -> sym -> st -- Transition function
    , st -- Initial state
    , [st] -- Final states
    )
--Definition of functions to get an element from a tuple by it's
--index.
get1:: (a,b,c,d,e)->a
get1 (a,_,_,_,_) = a
get2:: (a,b,c,d,e)->b
get2 (_,b,_,_,_) = b
get3:: (a,b,c,d,e)->c
get3 (_,_,c,_,_) = c
get4:: (a,b,c,d,e)->d
get4 (_,_,_,d,_) = d
get5:: (a,b,c,d,e)->e
get5 (_,_,_,_,e) = e
--Create checking function.
--This function receibes a DFA with its states and alphabet and
--returns a boolean value. It will return True if the language
--accepted by the DFA is empty and false otherwise.
--It is important to write the Eq typeclass in order to test for
--equality, in this case we test for equality when the intersect
--function is called.
--Taken from:
--http://learnyouahaskell.com/types-and-typeclasses
--This function defines acc as the acceptance states of the DFA
--and reachable as the accessible states from the initial state.
--After the accSates returns the list of accessible states,
--the null function returns True if the intesection between
--the set accepting states and accessible states is null.
isEmpty :: (Eq st, Eq sym) => DFA st sym -> Bool
isEmpty dfa =
    let acc = get5 dfa
        reachable = accStates dfa [] [get4 dfa]
    in null (acc `intersect` reachable)
--This function is defined in order to get the elements in
--xs that aren't in ys.
(\\) :: Eq a => [a] -> [a] -> [a]
xs \\ ys = [x | x <- xs, x `notElem` ys]
--The accStates function receives a DFA and two lists of states,
--the first list represents the reachable states and the second
--list represents an auxiliar list used to temporarly save which states is
--the function currently examining.The function is internally performing a 
--variation of the DFS algorithm in order to fill the reachable states list, 
--and then it returns it.
accStates :: Eq st => DFA st sym -> [st] -> [st] -> [st]
accStates dfa acc n =
    let t = get3 dfa
        visited = acc
        unvisited = n \\ visited
    in if null unvisited
            then acc ++ [get4 dfa]
            else 
                let n' = [t curr s | s <- get2 dfa, curr <- unvisited] 
                    acc' = visited ++ unvisited
                in accStates dfa acc' n'