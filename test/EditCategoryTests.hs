module EditCategoryTests where

import Data.Functor.Identity (Identity)
import Data.Time (getCurrentTime)
import Endpoints.Handlers.EditCategory (EditCategoryResult (..), Handle (..), hEditCategory)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Types.API.Category (EditCategoryRequest (..))
import qualified Types.DB.Category as DB (Category (..))
import qualified Types.DB.User as DB (User (..))

testHandle :: Handle Identity
testHandle =
  Handle
    { hReadCategoryById = \catId -> case catId of
        1 -> pure (Just sampleCategory1)
        4 -> pure (Just sampleDoughterCategory)
        42 -> pure Nothing,
      hReadCategoryByTitle = \title -> case title of
        "titleIsTaken" -> pure (Just sampleCategory2)
        _ -> pure Nothing,
      hRewriteCategory = \_ -> pure ()
    }

sampleAdminUser :: IO DB.User
sampleAdminUser = do
  now <- getCurrentTime
  pure $
    DB.User
      { userId = 1,
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

sampleCategory1 :: DB.Category
sampleCategory1 =
  DB.Category
    { categoryId = 1,
      title = "title1",
      parentId = Just 3
    }

sampleCategory2 :: DB.Category
sampleCategory2 =
  DB.Category
    { categoryId = 2,
      title = "titleIsTaken",
      parentId = Just 3
    }

sampleDoughterCategory :: DB.Category
sampleDoughterCategory =
  DB.Category
    { categoryId = 4,
      title = "Doughter",
      parentId = Just 1
    }

editCategoryRequest =
  EditCategoryRequest
    { processedCategoryId = 1,
      newTitle = Nothing,
      newParentCategoryId = Nothing
    }

editCategoryTest :: SpecWith ()
editCategoryTest =
  describe "editCategoryTests" $ do
    it "Shouldn't edit category if user is not admin" $ do
      invoker <- sampleNotAdminUser
      let result = hEditCategory testHandle invoker editCategoryRequest
      result `shouldBe` return NotAdmin
    it "Shouldn't edit category if there is no such category" $ do
      invoker <- sampleAdminUser
      let req = editCategoryRequest {processedCategoryId = 42}
      let result = hEditCategory testHandle invoker req
      result `shouldBe` return CategoryNotExists
    it "Shouldn't edit category if title is taken" $ do
      invoker <- sampleAdminUser
      let req = editCategoryRequest {newTitle = Just "titleIsTaken"}
      let result = hEditCategory testHandle invoker req
      result `shouldBe` return IncorrectTitle
    it "Shouldn't edit category if new parent category doesn't exist" $ do
      invoker <- sampleAdminUser
      let req = editCategoryRequest {newParentCategoryId = Just 42}
      let result = hEditCategory testHandle invoker req
      result `shouldBe` return IncorrectParentId
    it "Shouldn't edit category if new parent category is equal processed category" $ do
      invoker <- sampleAdminUser
      let req = editCategoryRequest {newParentCategoryId = Just 1}
      let result = hEditCategory testHandle invoker req
      result `shouldBe` return IncorrectParentId
    it "Shouldn't edit category if new parent category is a child of the processed category" $ do
      invoker <- sampleAdminUser
      let req = editCategoryRequest {newParentCategoryId = Just 4}
      let result = hEditCategory testHandle invoker req
      result `shouldBe` return IncorrectParentId
    it "Should successfully edit category" $ do
      invoker <- sampleAdminUser
      let result = hEditCategory testHandle invoker editCategoryRequest
      result `shouldBe` return EditCategorySuccess
