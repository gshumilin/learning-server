import Test.Hspec
import EditNewsTests
import EditCategoryTests

main :: IO ()
main = hspec $ do
  editNewsTest
  editCategoryTest
  