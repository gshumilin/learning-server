module EditNewsTests where

import Data.Functor.Identity (Identity)
import Data.Time (getCurrentTime)
import Endpoints.Handlers.EditNews (EditNewsResult (..), Handle (..), hEditNews)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Types.API.News (EditNewsRequest (..))
import Types.DB.News (EditedNewsFields (..))
import qualified Types.DB.User as DB (User (..))
import Types.Domain.User

testHandle :: Handle Identity
testHandle =
  Handle
    { hReadSpecificNews = \_ -> pure Nothing,
      hRewriteNews = \_ _ -> pure ()
    }

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

editNewsRequest =
  EditNewsRequest
    { newsId = 1,
      newTitle = Nothing,
      newCategoryId = Nothing,
      newTextContent = Nothing,
      newPictures = Nothing
    }

currNews =
  EditedNewsFields
    { oldCreatorId = 1,
      oldTitle = "T.Text",
      oldCategoryId = 1,
      oldTextContent = "T.Text"
    }

editNewsTest :: SpecWith ()
editNewsTest =
  describe "editNewsTests" $ do
    it "Shouldn't edit news if post doesn't exist" $ do
      invoker <- sampleUser
      let result = hEditNews testHandle invoker editNewsRequest
      result `shouldBe` pure NewsNotExists
    it "Should successfully edit post" $ do
      invoker <- sampleUser
      let successHandle = testHandle {hReadSpecificNews = \_ -> pure $ Just currNews}
      let result = hEditNews successHandle invoker editNewsRequest
      result `shouldBe` pure EditNewsSuccess
