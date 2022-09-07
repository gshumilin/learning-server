module CreateCategoryTests where

import Data.Functor.Identity
import Data.Time
import Endpoints.Handlers.CreateCategory
import Test.Hspec
import Types.API.Category
import qualified Types.DB.Category as DB (Category (..))
import qualified Types.DB.User as DB (User (..))

testHandle :: Handle Identity
testHandle =
  Handle
    { hReadCategoryById = \catId -> case catId of
        1 -> pure (Just sampleCategory)
        42 -> pure Nothing,
      hReadCategoryByTitle = \title -> case title of
        "titleIsTaken" -> pure (Just sampleCategory)
        _ -> pure Nothing,
      hWriteCategory = \_ -> pure ()
    }

sampleAdminUser :: IO DB.User
sampleAdminUser = do
  now <- getCurrentTime
  pure $
    DB.User
      { userID = 1,
        name = "Name",
        login = "login",
        password = "password",
        createDate = now,
        isAdmin = True,
        isAbleToCreateNews = True
      }

sampleNotAdminUser = do
  user <- sampleAdminUser
  let res = user {DB.isAdmin = False}
  pure res

sampleCategory :: DB.Category
sampleCategory =
  DB.Category
    { categoryID = 1,
      title = "titleIsTaken",
      parentID = Just 3
    }

createCategoryRequest =
  CreateCategoryRequest
    { title = "someTitle",
      parentCategoryID = Nothing
    }

createCategoryTest :: SpecWith ()
createCategoryTest =
  describe "createCategoryTests" $ do
    it "Shouldn't create category if user is not admin" $ do
      invoker <- sampleNotAdminUser
      let result = hCreateCategory testHandle invoker createCategoryRequest
      result `shouldBe` return NotAdmin
    it "Shouldn't create category if parent category doesn't exist" $ do
      invoker <- sampleAdminUser
      let req = createCategoryRequest {parentCategoryID = Just 42}
      let result = hCreateCategory testHandle invoker req
      result `shouldBe` return IncorrectParentId
    it "Shouldn't create category if title is taken" $ do
      invoker <- sampleAdminUser
      let req = createCategoryRequest {title = "titleIsTaken"}
      let result = hCreateCategory testHandle invoker req
      result `shouldBe` return IncorrectTitle
    it "Should successfully create category" $ do
      invoker <- sampleAdminUser
      let req = createCategoryRequest {parentCategoryID = Just 1}
      let result = hCreateCategory testHandle invoker req
      result `shouldBe` return CreateCategorySuccess
