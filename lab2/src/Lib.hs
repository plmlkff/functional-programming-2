module Lib
    ( someFunc
    ) where

import AVLSet;

someFunc :: IO ()
someFunc = do
    let tree1 = insert 4 (insert 6 (insert 5 (insert 3 (insert 7 (insert 1 Empty)))))
    putStrLn "Дерево:"
    print tree1

    putStrLn "Дерево после удаления:"
    let tree2 = delete tree1 1
    print tree2

    let stringTree = map' show tree2
    putStrLn "Дерево после map':"
    print stringTree

    let filteredTree = filter' (>"3") stringTree
    putStrLn "Дерево после filter':"
    print filteredTree

    let foldRight = foldr'' (+) 0 tree1
    putStrLn "Правая свертка':"
    print foldRight

    let foldLeft = foldl'' (+) 0 tree1
    putStrLn "Левая свертка':"
    print foldLeft
