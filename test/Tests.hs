import Test.HUnit (Assertion)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

testInsertUnique :: Assertion
testInsertUnique = do
    newSkp <- empty <$> getStdGen
    let skp = foldr (\x skp -> insert x x skp) newSkp [1 .. 100]
    listAssert map (`member` skp) [1 .. 100]

testInsertDuplicates :: Assertion
testInsertDuplicates = do
    newSkp <- empty <$> getStdGen
    let numInsertions = 10
        size = 30
        insertAgain i skp = do
            let skp' = foldr skp (\x skp -> insert x (x + i) skp) [0 .. size]
            listAssert map (`member` skp') [1 .. 10]
    foldr newSkp insertAgain [0 .. numInsertions]

tests :: TestTree
tests = testGroup "Data.SkipList.Pure" []
