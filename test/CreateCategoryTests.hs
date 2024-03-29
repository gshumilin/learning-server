{-# LANGUAGE LambdaCase #-}

module CreateCategoryTests where

import Data.Functor.Identity (Identity)
import Data.Time (getCurrentTime)
import Endpoints.Handlers.CreateCategory (CreateCategoryResult (..), Handle (..), hCreateCategory)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Types.API.Category (CreateCategoryRequest (..))
import qualified Types.DB.Category as DB (Category (..))
import qualified Types.DB.User as DB (User (..))

testHandle :: Handle Identity
testHandle =
  Handle
    { hReadCategoryById =
        \case
          1 -> pure (Just sampleCategory)
          _ -> pure Nothing,
      hReadCategoryByTitle =
        \case
          "titleIsTaken" -> pure (Just sampleCategory)
          _ -> pure Nothing,
      hWriteCategory = \_ -> pure 1
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

sampleCategory :: DB.Category
sampleCategory =
  DB.Category
    { categoryId = 1,
      title = "titleIsTaken",
      parentId = Just 3
    }

createCategoryRequest =
  CreateCategoryRequest
    { title = "someTitle",
      parentCategoryId = Nothing
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
      let req = createCategoryRequest {parentCategoryId = Just 42}
      let result = hCreateCategory testHandle invoker req
      result `shouldBe` return IncorrectParentId
    it "Shouldn't create category if title is taken" $ do
      invoker <- sampleAdminUser
      let req = createCategoryRequest {title = "titleIsTaken"}
      let result = hCreateCategory testHandle invoker req
      result `shouldBe` return IncorrectTitle
    it "Should successfully create category" $ do
      invoker <- sampleAdminUser
      let req = createCategoryRequest {parentCategoryId = Just 1}
      let result = hCreateCategory testHandle invoker req
      result `shouldBe` return (CreateCategorySuccess 1)
