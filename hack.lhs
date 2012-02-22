Program for playing Fallout 3's "terminal hacking" minigame

\begin{code}
module Main where
import List (sort)
import Maybe (fromJust, isNothing, isJust)

\end{code}

The set of available passwords is represented as a complete graph with a node for
each possible password and edges weighted by the number of characters two words
have in common. The graph itself is kept as a list of nodes, each of which has a
String (or other thing, really, but we will use Strings here) and a list of
adjacent Strings, each paired with the associated edge weight.

\begin{code}
data GNode a t = Node {content :: a, edges :: [(a, t)]}
  deriving (Eq, Read, Show)
type Graph a t = [GNode a t]
\end{code}

The cg_node function constructs a single node in the complete graph. It requires
a list of neighbors and a function which computes "distance" between graph
elements (again, in this case, we use the number of matched characters in two
Strings).

\begin{code}
cg_node :: Num t => a -> [a] -> (a -> a -> t) -> GNode a t
cg_node n [] _  = Node n []
cg_node n es wt = Node n $ zip es $ map (wt n) es
\end{code}

The comp_graph function calls out to a helper function which simply applies
cg_node to each entry in the given list. The helper, cg_help, keeps a separate
copy of the initial list so that already-handled nodes are not forgotten during
recursive calls.

\begin{code}
comp_graph :: (Eq a, Num t) => (a -> a -> t) -> [a] -> Graph a t
comp_graph _ []   = []
comp_graph wt ns = cg_help ns ns wt

cg_help :: (Eq a, Num t) => [a] -> [a] -> (a -> a -> t) -> Graph a t
cg_help [] _ _ = []
cg_help (n:ns) neighbors wt = (cg_node n (filter (/= n) neighbors) wt)
                            : (cg_help ns neighbors wt)
\end{code}

Here is the weighting function mentioned above. First, zipWith (==) constructs a
Bool list, with entries representing whether input list elements at that position
matched. Then, filter (== True) eliminates all mismatch entries. Finally, length
counts the number of remaning entries, i.e. the number of matches in the inputs.

\begin{code}
char_match :: Eq a => [a] -> [a] -> Int
char_match xs ys = length $ filter (== True) $ zipWith (==) xs ys
\end{code}

At each step in the game, the program considers the distribution of edge weights
for each remaining graph node. The goal is to arrive at the correct answer as
quickly as possible, so the program chooses as its password guess the node which
would provide the most information (i.e. the one whose incident edge weights show
the highest entropy).

\begin{code}
entropy :: (Ord a, Floating t) => [a] -> t
entropy [] = 0
entropy xs = let occurrences = (map snd (count_sorted (List.sort xs))) in
             let probabilities = map (/ (fromIntegral $ length xs)) occurrences in
             let plogps = map (\p -> - p * (log p / log 2)) probabilities in
                 foldr (+) 0 plogps
\end{code}

The entropy calculation relies on a function which counts duplicate entries in
a sorted list. This function, count_sorted, associates with each unique entry
(with uniqueness determined by the == operator as applied to that type) the
number of times it occurred in the list. A helper function, count_sorted_h, uses
an extra parameter to track what value was last seen and how many times it has
been seen so far. While comparison operators were needed by the entropy
function in order to sort the list, they are no longer needed, as count_sorted
does not depend on there being any actual ordering (it just requires that all
duplicates be consecutive and that duplication be detectible).

\begin{code}
count_sorted :: (Eq a, Num t) => [a] -> [(a, t)]
count_sorted [] = []
count_sorted (x:xs) = count_sorted_h xs (x,1)

count_sorted_h :: (Eq a, Num t) => [a] -> (a, t) -> [(a, t)]
count_sorted_h [] (x, ct) = [(x, ct)]
count_sorted_h (y:ys) (x, ct)
  | x == y  = count_sorted_h ys (x, ct+1)
  | x /= y  = (x, ct) : (count_sorted_h ys (y, 1))
\end{code}

Now we need a way to compute the entropy associated with the edge weights of a
graph node. With what we've already built, all that must be done here is simple
function composition.

\begin{code}
node_entropy :: (Eq a, Ord s, Floating t) => GNode a s -> t
node_entropy = entropy . map snd . edges
\end{code}

Selecting which password to guess is simple: pair each node in the graph with its
edge-weight entropy, and then select the one with the greatest entropy.

\begin{code}
password_guess :: (Eq a, Ord t) => Graph a t -> Maybe a
password_guess g = snd $ foldr guess_compare (-1, Nothing) $ 
                         zip (map node_entropy g) (map (Just . content) g)

guess_compare :: (Ord t) => (t, a) -> (t, a) -> (t, a)
guess_compare (k1, v1) (k2, v2)
  | k1 < k2   = (k2, v2)
  | otherwise = (k1, v1)
\end{code}

Now that we have a way to choose what password to guess, we need a way to make
use of the information given by trying that password. We can eliminate all nodes
that don't have the right edge-weight next to a certain node. First, we must
identify all nodes which we should keep.

\begin{code}
correct_weight :: (Eq a, Eq t) => Graph a t -> a -> t -> Maybe [a]
correct_weight [] _ _      = Nothing
correct_weight (n:ns) word match
  | (content n) == word    = Just $ map fst $ filter ((match ==) . snd) $ edges n
  | otherwise              = correct_weight ns word match
\end{code}

Now we can use comp_graph again to build the new graph.

The main function serves to prime the input/output for the main_loop function,
which repeatedly gives a guess, asks how correct it was, and then updates its
internal graph.

\begin{code}
main = do
        putStrLn "Enter list of possible passwords (in Haskell's list notation)"
        input <- getLine
        let init_list = read input :: [String]
        let graph = (comp_graph char_match init_list) :: Graph String Int
        main_loop graph

main_loop :: Graph String Int -> IO ()
main_loop graph = do
        let guess = password_guess graph
        if (isNothing guess) then do
                putStrLn "Graph is empty -- no possibilities left"
                return ()
        else do
                let final_guess = fromJust guess
                putStrLn $ "Best guess is " ++ final_guess
                putStrLn "How many match?"
                input <- getLine
                let match = read input :: Int
                let new_words = correct_weight graph final_guess match
                if (isNothing new_words) then do
                        putStrLn "No possibilities left"
                        return ()
                else if (1 == length (fromJust new_words)) then do
                        putStrLn $ "Final possibility is " ++ (head (fromJust new_words))
                else do
                        let final_new_words = fromJust new_words
                        let new_graph = comp_graph char_match final_new_words
                        main_loop new_graph
\end{code}

