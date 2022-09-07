module CreateUserTests where

import Data.Functor.Identity (Identity)
import Data.Time (getCurrentTime)
import Endpoints.Handlers.CreateUser (CreateUserResult (..), Handle (..), hCreateUser)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Types.API.User (CreateUserRequest (..))
import qualified Types.DB.User as DB (User (..))

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

createUserRequest =
  CreateUserRequest
    { reqName = "name",
      reqLogin = "login",
      reqPassword = "password",
      reqIsAdmin = True,
      reqIsAbleToCreateNews = True
    }

testHandle :: Handle Identity
testHandle =
  Handle
    { hFindUserByLogin = \l ->
        case l of
          "takenLogin" -> pure (Just 2)
          _ -> pure Nothing,
      hWriteUser = \_ -> pure ()
    }

createUserTest :: SpecWith ()
createUserTest =
  describe "createUserTests" $ do
    it "Shouldn't create user if invoker is not admin" $ do
      invoker' <- sampleAdminUser
      let invoker = invoker' {DB.isAdmin = False}
      let result = hCreateUser testHandle invoker createUserRequest
      result `shouldBe` return NotAdmin
    it "Shouldn't create user if login is taken" $ do
      invoker <- sampleAdminUser
      let req = createUserRequest {reqLogin = "takenLogin"}
      let result = hCreateUser testHandle invoker req
      result `shouldBe` return LoginIsTaken
    it "Should create user" $ do
      invoker <- sampleAdminUser
      let result = hCreateUser testHandle invoker createUserRequest
      result `shouldBe` return CreateUserSuccess
