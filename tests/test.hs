import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Grid.Tests
import qualified Hexagon.Tests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Hexagon.Tests.tests
    , Grid.Tests.tests
    ]
