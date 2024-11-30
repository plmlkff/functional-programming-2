{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Test.Hspec
import Test.Validity
import Test.QuickCheck (quickCheckWith, stdArgs, maxSuccess)
import AVLSet;

-- Property-based tests for `AVLTree` operations
spec :: Spec
spec = do
  describe "AVLTree" $ do
    -- Validity property tests
    it "ensures that empty AVLTree is valid" $
      shouldBeValid (Empty :: AVLTree Char)

    it "ensures that insert maintains validity" $
      quickCheckWith (stdArgs { maxSuccess = 10 }) $ forAllValid $ \(word :: Int) ->
        forAllValid $ \tree ->
          shouldBeValid (insert word tree)

    it "ensures that remove maintains validity" $
      quickCheckWith (stdArgs { maxSuccess = 10 }) $ forAllValid $ \(word :: Char) ->
        forAllValid $ \tree ->
          shouldBeValid (delete tree word)

    it "ensures that filter maintains validity" $
      quickCheckWith (stdArgs { maxSuccess = 10 }) $ forAllValid $ \(tree :: AVLTree Int) ->
        shouldBeValid (filter' ( > 2) tree)

    it "ensures that toList maintains validity" $
      quickCheckWith (stdArgs { maxSuccess = 10 }) $ forAllValid $ \(tree :: AVLTree Char) ->
        shouldBeValid (toList tree)

    it "ensures that order of insertions does not matter" $ do
        let tree1 = insert 4 (insert 6 (insert 5 (insert 3 (insert 7 (insert 1 Empty)))))
        let tree2 = insert 6 (insert 4 (insert 5 (insert 7 (insert 3 (insert 1 Empty)))))
        toList tree1 `shouldBe` toList tree2

        let tree1Deleted = delete (delete tree1 5) 4
        let tree2Deleted = delete (delete tree1 4) 5
        toList tree1 `shouldBe` toList tree2


    it "unit testing tree, polymorphic test" $ do
        let tree1 = insert 4 (insert 6 (insert 5 (insert 3 (insert 7 (insert 1 Empty)))))
        show tree1 `shouldBe` "Node 5 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 6 Empty (Node 7 Empty Empty))"

        let tree2 = delete tree1 1
        toList tree2 `shouldBe` [3, 4, 5, 6, 7]

        let stringTree = map' show tree2
        toList stringTree `shouldBe` ["3", "4", "5", "6", "7"]

        let filteredTree = filter' (>"3") stringTree
        toList filteredTree `shouldBe` ["4", "5", "6", "7"]

        let foldRight = foldr'' (+) 0 tree1
        foldRight `shouldBe` 26

        let foldLeft = foldl'' (+) 0 tree1
        foldLeft `shouldBe` 26

-- Helper to run the tests using hspec
main :: IO ()
main = hspec spec