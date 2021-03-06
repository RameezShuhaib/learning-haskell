import TheoryTestSuite
import CSVParserTestSuite
import Test.Tasty (TestTree, defaultMain, testGroup)

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Haskell Lecture Series - Shuttl"
    [ lecture03TheoryTestSuite
    , csvParserTestSuite
    ]
