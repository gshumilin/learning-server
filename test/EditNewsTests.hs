module EditNewsTests where

import Data.Time
import Endpoints.Handlers.EditNews
import Types.Domain.User
import qualified Types.Database.User as DB (User(..)) 
import Types.API.News
import Types.Database.News (EditedNewsFields(..))
import Test.Hspec
import Data.Functor.Identity

testHandle :: Handle Identity
testHandle = Handle
  { hReadSpecificNews = \_ -> pure Nothing,
    hRewriteNews = \_ _ -> pure ()
  }

sampleUser :: IO DB.User
sampleUser = do
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

editNewsRequest = EditNewsRequest
    { newsID = 1,
      newTitle = Nothing,
      newCategoryID = Nothing,
      newTextContent = Nothing,
      newPictures = Nothing
    }

currNews = EditedNewsFields
    { oldCreatorID = 1,
      oldTitle = "T.Text",
      oldCategoryID = 1,
      oldTextContent = "T.Text"
    }

editNewsTest :: SpecWith ()
editNewsTest = 
  describe "editNewsTests" $ do
    it "Shouldn't edit news if post doesn't exist" $ do
      invoker <- sampleUser
      let result = hEditNews testHandle invoker editNewsRequest
      result `shouldBe` return NewsNotExists
    it "Should successfully edit post" $ do
      invoker <- sampleUser
      let successHandle = testHandle {hReadSpecificNews = \_ -> return $ Just currNews}
      let result = hEditNews successHandle invoker editNewsRequest
      result `shouldBe` return EditNewsSuccess