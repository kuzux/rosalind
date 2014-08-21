import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.IO as TIO
import Newick

numberInternalNodes :: Tree -> Tree
numberInternalNodes t = evalState (comp t) 1 
    where comp :: Tree -> State Int Tree
          comp l@(Leaf _)      = return l
          comp (Internal _ cs) = do n <- gets $ Just . show
                                    modify succ
                                    cs' <- mapM comp cs
                                    return (Internal n cs')

-- assumptions : t is an unrooted binary tree with no unlabeled nodes
-- invariants : n = number of internal nodes in t => number of characters is n-1
characterTrees :: Tree -> [(Tree, Tree)]
characterTrees t = execWriter $ runStateT (comp t) Set.empty
    where comp :: Tree -> StateT (Set String) (Writer [(Tree, Tree)]) ()
          comp (Leaf _) = return ()
          comp t@(Internal n cs) = do visited <- get
                                      let visited' = Set.insert (label t) visited
                                      let toVisit = filter (visitable visited) cs
                                      let cs' = map (rebase t) toVisit
                                      put visited'
                                      mapM_ visit cs'
          
          visitable :: Set String -> Tree -> Bool
          visitable set x = not $ isLeaf x || Set.member (label x) set

          visit :: (Tree,Tree,Tree) -> StateT (Set String) (Writer [(Tree, Tree)]) ()
          visit (a,b,b') = do tell [(a,b)]
                              comp b'

          isLeaf :: Tree -> Bool
          isLeaf (Leaf _) = True
          isLeaf _        = False

          label :: Tree -> String
          label (Leaf n) = fromJust n
          label (Internal n _) = fromJust n

          -- split a b
          -- both a and b should be internal nodes
          -- b should be a direct child of a
          -- it returns (a without b, b, b with a)
          rebase :: Tree -> Tree -> (Tree, Tree, Tree)
          rebase a@(Internal n1 c1) b@(Internal n2 c2) = (t1, b, t2)
                    where t1 = Internal n1 $ filter (\x -> label x /= fromJust n2) c1
                          t2 = Internal n2 $ t1:c2 
          rebase a b = error $ show a ++ " " ++ show b

characterSets :: [(Tree, Tree)] -> [(Set String, Set String)]
characterSets = map go 
    where go (a, b) = (Set.fromList . leafLabels $ a, Set.fromList . leafLabels $ b)

leafLabels :: Tree -> [String]
leafLabels (Leaf l) = maybeToList l
leafLabels (Internal _ cs) = concat $ map leafLabels cs

sharedSplits :: [(Set String, Set String)] -> [(Set String, Set String)] -> Int
sharedSplits sp1 sp2 = length $ filter (contains sp2) sp1
    where contains xs (x,y) = (x,y) `elem` xs || (y,x) `elem` xs

main :: IO ()
main = do labels <- words <$> getLine
          let n = length labels
          tree1 <- either error id . parseTree <$> TIO.getLine
          tree2 <- either error id . parseTree <$> TIO.getLine
          let characters1 = characterSets . characterTrees . numberInternalNodes $ tree1
          let characters2 = characterSets . characterTrees . numberInternalNodes $ tree2
          let shared = sharedSplits characters1 characters2
          print $ 2*(n-3) - 2*shared
