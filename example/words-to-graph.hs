#!/usr/bin/env stack
-- stack runghc
module Main(main) where

import           Control.Exception  (evaluate)
import           System.Environment
import           System.IO

import           Data.ADFA
import           Data.List

main :: IO ()
main =
  do str <- withWordsFile $ \handle ->
       do s <- hGetContents handle
          _ <- evaluate $ length s
          return s
     let dfa = topSort . minify . strings $ lines str
     putStrLn $ printAsDot (:[]) dfa
     return ()

withWordsFile :: (Handle -> IO a) -> IO a
withWordsFile f =
  do args <- getArgs
     case args of
       []       -> f stdin
       (file:_) -> withFile file ReadMode f

printAsDot :: (c -> String) -> ADFA c -> String
printAsDot showC dfa =
    mkDigraph (nodeStmts ++ edgeStmts)
  where
    nodes = nodeList dfa
    edges = edgeList dfa
    root = rootNode dfa

    mkDigraph stmts = unlines $
      "digraph {" : map ("  " ++) stmts ++ ["}"]
    nodeName n = "n" ++ show n

    nodeStmts =
      let (acceptedNodes, rejectedNodes) = partition snd $ filter ((/= root) . fst) nodes
          [(_,acceptsRoot)] = filter ((==root) . fst) nodes
          def1 (n,_) = nodeName n ++ " [label=\"" ++ show n ++ "\"];"
          rootNumLine = if acceptsRoot then 2 else 1 :: Int
          rootAttr = "node [shape=box, peripheries=" ++ show rootNumLine ++ "];"
      in rootAttr : def1 (root, acceptsRoot) :
         "node [shape=circle];" : map def1 rejectedNodes ++
         "node [shape=circle, peripheries=2];" : map def1 acceptedNodes
    
    edgeStmts =
      let def1 (n, c, n') = nodeName n ++ " -> " ++ nodeName n' ++
                            " [label=\"" ++ showC c ++ "\"];"
      in map def1 edges
