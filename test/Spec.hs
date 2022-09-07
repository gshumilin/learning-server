import CreateCategoryTests
import CreateNewsTests
import EditCategoryTests
import EditNewsTests
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  editNewsTest
  editCategoryTest
  createCategoryTest
  createNewsTest
