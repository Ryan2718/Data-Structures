-- Ryan Forsyth
-- 04/14/2015

module Main where

import Graph

e1 = Edge "A" "B" 1
e2 = Edge "B" "C" 2
e3 = Edge "C" "A" 3

directed = insertDirected e2 $ insertDirected e1 $ Graph []
directedRemoved = removeDirected e2 directed

test01 = True == graphContainsVertex "A" directed
test02 = True == graphContainsVertex "B" directed
test03 = True == graphContainsVertex "C" directed

test04 = True == graphContainsVertex "A" directedRemoved
test05 = True == graphContainsVertex "B" directedRemoved
test06 = False == graphContainsVertex "C" directedRemoved

test07 = True == containsEdge e1 directed
test08 = True == containsEdge e2 directed
test09 = False == containsEdge e3 directed

test10 = True == containsEdge e1 directedRemoved
test11 = False == containsEdge e2 directedRemoved
test12 = False == containsEdge e3 directedRemoved

main = do
          if test01 then return() else putStrLn "Failed test01"
          if test02 then return() else putStrLn "Failed test02"
          if test03 then return() else putStrLn "Failed test03"
          if test04 then return() else putStrLn "Failed test04"
          if test05 then return() else putStrLn "Failed test05"
          if test06 then return() else putStrLn "Failed test06"
          if test07 then return() else putStrLn "Failed test07"
          if test08 then return() else putStrLn "Failed test08"
          if test09 then return() else putStrLn "Failed test09"
          if test10 then return() else putStrLn "Failed test10"
          if test11 then return() else putStrLn "Failed test11"
          if test12 then return() else putStrLn "Failed test12"
          putStrLn "Finished"
