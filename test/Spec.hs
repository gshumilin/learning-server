import EditCategoryTests
import EditNewsTests
import Test.Hspec

main :: IO ()
main = hspec $ do
  editNewsTest
  editCategoryTest
