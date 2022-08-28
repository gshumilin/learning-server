module EditCategoryTests where

import Data.Time
import Endpoints.Handlers.EditCategory
import qualified Types.Database.User as DB (User(..)) 
import Types.API.Category
import qualified Types.Database.Category as DB (Category(..))
import Test.Hspec
import Data.Functor.Identity

testHandle :: Handle Identity
testHandle = Handle
  { hReadCategoryById = \catId -> case catId of
      1 -> pure (Just sampleCategory1)
      4 -> pure (Just sampleDoughterCategory)
      42 -> pure (Nothing),
    hReadCategoryByTitle = \title -> case title of 
      "titleIsTaken" -> pure (Just sampleCategory2)
      _ -> pure Nothing,
    hRewriteCategory = \_ -> pure ()
  }

sampleAdminUser :: IO DB.User
sampleAdminUser = do
  now <- getCurrentTime
  return $ DB.User
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
  let res = user {DB.isAdmin=False}
  pure res

sampleCategory1 :: DB.Category
sampleCategory1 = DB.Category
  { categoryID = 1,
    title = "title1",
    parentID = Just 3
  }

sampleCategory2 :: DB.Category
sampleCategory2 = DB.Category
  { categoryID = 2,
    title = "titleIsTaken",
    parentID = Just 3
  }  

sampleDoughterCategory :: DB.Category
sampleDoughterCategory = DB.Category
  { categoryID = 4,
    title = "Doughter",
    parentID = Just 1
  }  

editCategoryRequest = EditCategoryRequest
  { processedCategoryID = 1,
    newTitle = Nothing ,
    newParentCategoryID = Nothing
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
      let req = editCategoryRequest {processedCategoryID = 42}
      let result = hEditCategory testHandle invoker req
      result `shouldBe` return CategoryNotExists
    it "Shouldn't edit category if title is taken" $ do
      invoker <- sampleAdminUser
      let req = editCategoryRequest {newTitle = Just "titleIsTaken"}
      let result = hEditCategory testHandle invoker req
      result `shouldBe` return IncorrectTitle
    it "Shouldn't edit category if new parent category doesn't exist" $ do
      invoker <- sampleAdminUser
      let req = editCategoryRequest {newParentCategoryID = Just 42}
      let result = hEditCategory testHandle invoker req
      result `shouldBe` return IncorrectParentId
    it "Shouldn't edit category if new parent category is equal processed category" $ do
      invoker <- sampleAdminUser
      let req = editCategoryRequest {newParentCategoryID = Just 1}
      let result = hEditCategory testHandle invoker req
      result `shouldBe` return IncorrectParentId
    it "Shouldn't edit category if new parent category is a child of the processed category" $ do
      invoker <- sampleAdminUser
      let req = editCategoryRequest {newParentCategoryID = Just 4}
      let result = hEditCategory testHandle invoker req
      result `shouldBe` return IncorrectParentId
    it "Should successfully edit category" $ do
      invoker <- sampleAdminUser
      let result = hEditCategory testHandle invoker editCategoryRequest
      result `shouldBe` return EditCategorySuccess