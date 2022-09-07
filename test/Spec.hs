import CreateCategoryTests
import CreateNewsTests
import CreateUserTests
import EditCategoryTests
import EditNewsTests
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  editNewsTest
  editCategoryTest
  createCategoryTest
  createNewsTest
  createUserTest
