{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Concurrent       as Concurrent
import qualified Data.Aeson               as Aeson
import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Network.HTTP.Client      as HTTP
import qualified Network.HTTP.Types       as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import qualified Test.Hspec               as Hspec

import           Data.Acid                (AcidState, openLocalState)
import           Data.IORef               (newIORef, readIORef, writeIORef)
import           GHC.Generics             (Generic)
import qualified Lib                      as L
import           Network.Socket           (SockAddr (SockAddrInet), addrAddress,
                                           close, defaultHints, getAddrInfo,
                                           openSocket, withSocketsDo)
import           Network.Wai              (Application)
import           Servant
import           Servant.Client

data LoginResponse = LoginResponse
  { tokenId      :: T.Text
  , tokenUser    :: T.Text
  , tokenExpires :: T.Text
  } deriving (Show, Generic)

instance Aeson.FromJSON LoginResponse
instance Aeson.ToJSON LoginResponse

main :: IO ()
main = withSocketsDo $ do
  let port = 8080
  acidState <- openLocalState L.initialAppState
  let app = serve (Proxy :: Proxy L.API) (L.server acidState)

  serverThread <- Concurrent.forkIO $ Warp.run port $ L.rejectUnknownParams ["from", "mentions", "tags"] app

  Concurrent.threadDelay 100000

  Hspec.hspec $ do
    Hspec.beforeAll (return ()) $
        Hspec.afterAll (\_ -> Concurrent.killThread serverThread) $ do
        authSpec port
        tweetSpec port


localBaseUrl :: Int -> String
localBaseUrl port = "http://localhost:" ++ show port

createClient :: Int -> IO HTTP.Manager
createClient port = HTTP.newManager HTTP.defaultManagerSettings

testUsername :: T.Text
testUsername = "testuser"

testPassword :: T.Text
testPassword = "mypassword"

invalidUsername :: T.Text
invalidUsername = "1invalid"

testTweetContent :: T.Text
testTweetContent = "My first tweet! #Haskell"

updatedTweetContent :: T.Text
updatedTweetContent = "Updated tweet content #Programming"

authSpec :: Int -> Hspec.Spec
authSpec port = Hspec.describe "Authentication" $ do
  Hspec.it "should register a new user" $ do
    manager <- createClient port
    let registerUrl = localBaseUrl port ++ "/auth/register"
    request <- HTTP.parseRequest registerUrl
    let reqBody = Aeson.encode [testUsername, testPassword]
    let req = request {
          HTTP.method = "POST",
          HTTP.requestHeaders = [("Content-Type", "application/json")],
          HTTP.requestBody = HTTP.RequestBodyLBS reqBody
        }

    response <- HTTP.httpLbs req manager
    HTTP.responseStatus response `Hspec.shouldBe` HTTP.status200

  Hspec.it "should reject invalid username format" $ do
    manager <- createClient port
    let registerUrl = localBaseUrl port ++ "/auth/register"
    request <- HTTP.parseRequest registerUrl
    let reqBody = Aeson.encode [invalidUsername, testPassword]
    let req = request {
          HTTP.method = "POST",
          HTTP.requestHeaders = [("Content-Type", "application/json")],
          HTTP.requestBody = HTTP.RequestBodyLBS reqBody
        }

    response <- HTTP.httpLbs req manager
    HTTP.responseStatus response `Hspec.shouldBe` HTTP.status400

  Hspec.it "should login with valid credentials" $ do
    manager <- createClient port
    let loginUrl = localBaseUrl port ++ "/auth/login"
    request <- HTTP.parseRequest loginUrl
    let reqBody = Aeson.encode [testUsername, testPassword]
    let req = request {
          HTTP.method = "POST",
          HTTP.requestHeaders = [("Content-Type", "application/json")],
          HTTP.requestBody = HTTP.RequestBodyLBS reqBody
        }

    response <- HTTP.httpLbs req manager
    HTTP.responseStatus response `Hspec.shouldBe` HTTP.status200
    let mToken = Aeson.decode (HTTP.responseBody response) :: Maybe LoginResponse
    case mToken of
        Nothing -> Hspec.expectationFailure "Failed to decode auth token"
        Just token -> do
            tokenId token `Hspec.shouldSatisfy` (not . T.null)
            tokenUser token `Hspec.shouldBe` testUsername
            tokenExpires token `Hspec.shouldSatisfy` (not . T.null)

  Hspec.it "should reject invalid credentials" $ do
    manager <- createClient port
    let loginUrl = localBaseUrl port ++ "/auth/login"
    request <- HTTP.parseRequest loginUrl
    let reqBody = Aeson.encode [testUsername, "wrongpassword"]
    let req = request {
          HTTP.method = "POST",
          HTTP.requestHeaders = [("Content-Type", "application/json")],
          HTTP.requestBody = HTTP.RequestBodyLBS reqBody
        }

    response <- HTTP.httpLbs req manager
    HTTP.responseStatus response `Hspec.shouldBe` HTTP.status401

tweetSpec :: Int -> Hspec.Spec
tweetSpec port = do
  tokenRef <- Hspec.runIO $ newIORef Nothing

  Hspec.beforeAll (do
    manager <- createClient port
    let loginUrl = localBaseUrl port ++ "/auth/login"
    request <- HTTP.parseRequest loginUrl
    let reqBody = Aeson.encode [testUsername, testPassword]
    let req = request {
          HTTP.method = "POST",
          HTTP.requestHeaders = [("Content-Type", "application/json")],
          HTTP.requestBody = HTTP.RequestBodyLBS reqBody
        }

    response <- HTTP.httpLbs req manager
    let mToken = Aeson.decode (HTTP.responseBody response) :: Maybe LoginResponse
    case mToken of
      Just token -> writeIORef tokenRef (Just (tokenId token))
      Nothing    -> Hspec.expectationFailure "Failed to get auth token"
    ) $ do

    Hspec.it "should create a new tweet" $ do

      mToken <- readIORef tokenRef
      case mToken of
        Nothing -> Hspec.expectationFailure "No auth token available"
        Just token -> do
          manager <- createClient port
          let tweetUrl = localBaseUrl port ++ "/send"
          request <- HTTP.parseRequest tweetUrl
          let req = request {
                HTTP.method = "POST",
                HTTP.requestHeaders = [
                  ("Content-Type", "text/plain; charset=utf-8"),
                  ("Authorization", BS.pack $ T.unpack token)
                ],
                HTTP.requestBody = HTTP.RequestBodyLBS (BSL.fromStrict $ TE.encodeUtf8 testTweetContent)
              }

          response <- HTTP.httpLbs req manager
          HTTP.responseStatus response `Hspec.shouldBe` HTTP.status200

    Hspec.it "should update a tweet" $ do
      mToken <- readIORef tokenRef
      case mToken of
        Nothing -> Hspec.expectationFailure "No auth token available"
        Just token -> do
          manager <- createClient port
          let updateUrl = localBaseUrl port ++ "/tweets/1"
          request <- HTTP.parseRequest updateUrl
          let req = request {
                HTTP.method = "PUT",
                HTTP.requestHeaders = [
                  ("Content-Type", "text/plain; charset=utf-8"),
                  ("Authorization", BS.pack $ T.unpack token)
                ],
                HTTP.requestBody = HTTP.RequestBodyLBS (BSL.fromStrict $ TE.encodeUtf8 updatedTweetContent)
              }

          response <- HTTP.httpLbs req manager
          HTTP.responseStatus response `Hspec.shouldBe` HTTP.status200

    Hspec.it "should search for tweets" $ do
      manager <- createClient port
      let searchUrl = localBaseUrl port ++ "/search?tags=Programming"
      request <- HTTP.parseRequest searchUrl
      let req = request { HTTP.method = "GET" }

      response <- HTTP.httpLbs req manager
      HTTP.responseStatus response `Hspec.shouldBe` HTTP.status200
      let mTweets = Aeson.decode (HTTP.responseBody response) :: Maybe [Aeson.Value]
      case mTweets of
        Nothing     -> Hspec.expectationFailure "Failed to decode tweets"
        Just tweets -> length tweets `Hspec.shouldSatisfy` (== 1)

    Hspec.it "should delete a tweet" $ do
      mToken <- readIORef tokenRef
      case mToken of
        Nothing -> Hspec.expectationFailure "No auth token available"
        Just token -> do
          manager <- createClient port
          let deleteUrl = localBaseUrl port ++ "/tweets/1"
          request <- HTTP.parseRequest deleteUrl
          let req = request {
                HTTP.method = "DELETE",
                HTTP.requestHeaders = [
                  ("Authorization", BS.pack $ T.unpack token)
                ]
              }

          response <- HTTP.httpLbs req manager
          HTTP.responseStatus response `Hspec.shouldBe` HTTP.status200
