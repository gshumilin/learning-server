module CreateNewsTests where

import Data.Functor.Identity (Identity)
import Data.Time (getCurrentTime)
import Endpoints.Handlers.CreateNews (CreateNewsResult (..), Handle (..), hCreateNews)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Types.API.News (CreateNewsRequest (..))
import qualified Types.DB.Category as DB (Category (..))
import qualified Types.DB.User as DB (User (..))
import Types.Domain.Picture (Picture (..))

sampleUser :: IO DB.User
sampleUser = do
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

sampleCategory :: DB.Category
sampleCategory =
  DB.Category
    { categoryId = 1,
      title = "title1",
      parentId = Just 3
    }

createNewsRequest :: CreateNewsRequest
createNewsRequest =
  CreateNewsRequest
    { title = "someTitle",
      categoryId = 1,
      textContent = "someContent",
      pictures = Nothing
    }

testHandle :: Handle Identity
testHandle =
  Handle
    { hReadCategoryById = \cId ->
        case cId of
          1 -> pure (Just sampleCategory)
          _ -> pure Nothing,
      hFindNewsIdByTitle = \t ->
        case t of
          "invalid" -> pure $ Just 42
          _ -> pure Nothing,
      hWriteNews = \_ _ -> pure 1
    }

badPictures :: [Picture]
badPictures =
  [ Picture "image/pNg" "somePicData",
    Picture "gif" "someGifData"
  ]

createNewsTest :: SpecWith ()
createNewsTest =
  describe "createNewsTests" $ do
    it "Shouldn't create news if invoker is not able to create news" $ do
      invoker' <- sampleUser
      let invoker = invoker' {DB.isAbleToCreateNews = False}
      let result = hCreateNews testHandle invoker createNewsRequest
      result `shouldBe` pure NotAbleToCreateNews
    it "Shouldn't create news if title is invalid" $ do
      invoker <- sampleUser
      let req = createNewsRequest {title = "invalid"}
      let result = hCreateNews testHandle invoker req
      result `shouldBe` pure IncorrectTitle
    it "Shouldn't create news if pictures format is invalid" $ do
      invoker <- sampleUser
      let req = createNewsRequest {pictures = Just badPictures}
      let result = hCreateNews testHandle invoker req
      result `shouldBe` pure InvalidPictureFormat
    it "Shouldn't create news if there is no such category" $ do
      invoker <- sampleUser
      let req = createNewsRequest {categoryId = 42}
      let result = hCreateNews testHandle invoker req
      result `shouldBe` pure CategoryNotExists
    it "Should succwssfully create news" $ do
      invoker <- sampleUser
      let result = hCreateNews testHandle invoker createNewsRequest
      result `shouldBe` pure (CreateNewsSuccess 1)
