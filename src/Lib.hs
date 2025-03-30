{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Lib where

import           Control.Monad            (when, unless)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader     (ask, asks)
import           Control.Monad.State      (get, put)
import           Crypto.BCrypt            (hashPassword, validatePassword)
import           Data.Acid
import           Data.Acid.Advanced       (query', update')
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Char8    as BSC
import           Data.List                (find, isInfixOf)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe, isNothing)
import           Data.SafeCopy            (SafeCopy, base, deriveSafeCopy)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Data.Time.Clock          (UTCTime, addUTCTime, diffUTCTime,
                                           getCurrentTime)
import           Data.Time.Format         (defaultTimeLocale, formatTime)
import           GHC.Generics             (Generic)
import           Network.HTTP.Types       (status400, status401, status403,
                                           status404)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.Server           (err400, err401, err403, err404)
import           System.Random            (randomIO)
import           Text.Regex.TDFA          ((=~))

data Tweet = Tweet
  { tweetId   :: Int
  , author    :: Text
  , content   :: Text
  , timestamp :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON Tweet
instance FromJSON Tweet

data TweetsState = TweetsState
  { tweets      :: [Tweet]
  , nextTweetId :: Int
  } deriving (Show)

initialState :: TweetsState
initialState = TweetsState [] 1

-- Структуры для аутентификации
data User = User
  { username :: Text
  , password :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON User
instance FromJSON User

data AuthToken = AuthToken
  { tokenId      :: Text
  , tokenUser    :: Text
  , tokenExpires :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON AuthToken
instance FromJSON AuthToken

-- Объединенное состояние приложения
data AppState = AppState
  { appTweets :: TweetsState
  , appUsers  :: Map Text User
  , appTokens :: Map Text AuthToken
  } deriving (Show)

initialAppState :: AppState
initialAppState = AppState initialState Map.empty Map.empty

-- Acid-state операции
addTweet :: Tweet -> Update AppState ()
addTweet tweet = do
  s <- get
  let ts = appTweets s
  put $ s { appTweets = ts { tweets = tweet : tweets ts } }

getTweets :: Query AppState [Tweet]
getTweets = do
  asks (tweets . appTweets)

getNextTweetId :: Query AppState Int
getNextTweetId = do
  asks (nextTweetId . appTweets)

incrementTweetId :: Update AppState ()
incrementTweetId = do
  s <- get
  let ts = appTweets s
  put $ s { appTweets = ts { nextTweetId = nextTweetId ts + 1 } }

updateTweet :: Int -> Text -> Text -> Update AppState Bool
updateTweet idToUpdate newContent requestingUser = do
  s <- get
  let ts = appTweets s
  case find (\t -> tweetId t == idToUpdate && author t == requestingUser) (tweets ts) of
    Just oldTweet -> do
      let updatedTweet = oldTweet { content = newContent }
          newTweets = updatedTweet : filter (\t -> tweetId t /= idToUpdate) (tweets ts)
      put $ s { appTweets = ts { tweets = newTweets } }
      return True
    Nothing -> return False

deleteTweet :: Int -> Text -> Update AppState Bool
deleteTweet idToDelete requestingUser = do
  s <- get
  let ts = appTweets s
  case find (\t -> tweetId t == idToDelete && author t == requestingUser) (tweets ts) of
    Just _ -> do
      let newTweets = filter (\t -> tweetId t /= idToDelete) (tweets ts)
      put $ s { appTweets = ts { tweets = newTweets } }
      return True
    Nothing -> return False

-- Acid-state операции для аутентификации
addUser :: User -> Update AppState ()
addUser user = do
  s <- get
  put $ s { appUsers = Map.insert (username user) user (appUsers s) }

getUser :: Text -> Query AppState (Maybe User)
getUser username = do
  asks (Map.lookup username . appUsers)

addAuthToken :: AuthToken -> Update AppState ()
addAuthToken token = do
  s <- get
  put $ s { appTokens = Map.insert (tokenId token) token (appTokens s) }

getAuthToken :: Text -> Query AppState (Maybe AuthToken)
getAuthToken tokenId = do
  asks (Map.lookup tokenId . appTokens)

getAllTokens :: Query AppState (Map Text AuthToken)
getAllTokens = asks appTokens

removeAuthToken :: Text -> Update AppState ()
removeAuthToken tokenId = do
  s <- get
  put $ s { appTokens = Map.delete tokenId (appTokens s) }

$(deriveSafeCopy 0 'base ''Tweet)
$(deriveSafeCopy 0 'base ''TweetsState)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''AuthToken)
$(deriveSafeCopy 0 'base ''AppState)

$(makeAcidic ''AppState [
    'addTweet, 'getTweets, 'getNextTweetId, 'incrementTweetId, 'updateTweet, 'deleteTweet,
    'addUser, 'getUser, 'addAuthToken, 'getAuthToken, 'getAllTokens, 'removeAuthToken
  ])

-- Вспомогательные функции для аутентификации
generateToken :: Text -> IO AuthToken
generateToken username = do
  token <- randomIO :: IO Int
  currentTime <- getCurrentTime
  let expiry = addUTCTime 3600 currentTime  -- 1 hour
  return $ AuthToken (T.pack $ show token) username expiry

isTokenValid :: AuthToken -> IO Bool
isTokenValid token = do
  currentTime <- getCurrentTime
  return $ tokenExpires token > currentTime

-- hashPassword' :: Text -> IO (Maybe Text)
-- hashPassword' password = do
--   let passwordBS = BSC.pack $ T.unpack password
--   mHash <- hashPassword 12 passwordBS
--   return $ fmap T.pack (BSC.unpack <$> mHash)

validatePassword' :: Text -> Text -> Bool
validatePassword' inputPassword storedPassword = inputPassword == storedPassword

type API =
       "auth" :> "register" :> ReqBody '[JSON] (Text, Text) :> Post '[JSON] NoContent
  :<|> "auth" :> "login" :> ReqBody '[JSON] (Text, Text) :> Post '[JSON] AuthToken
  :<|> "auth" :> "logout" :> Header "Authorization" Text :> Post '[JSON] NoContent
  :<|> "send" :> Header "Authorization" Text :> ReqBody '[PlainText] Text :> Post '[JSON] NoContent
  :<|> "search" :> QueryParams "tags" Text :> QueryParams "from" Text :> QueryParams "mentions" Text :> Get '[JSON] [Tweet]
  :<|> "tweets" :> Capture "tweetId" Int :> Header "Authorization" Text :> ReqBody '[PlainText] Text :> Put '[JSON] NoContent
  :<|> "tweets" :> Capture "tweetId" Int :> Header "Authorization" Text :> Delete '[JSON] NoContent

server :: AcidState AppState -> Server API
server acid = registerHandler acid
         :<|> loginHandler acid
         :<|> logoutHandler acid
         :<|> sendTweetHandler acid
         :<|> searchTweetsHandler acid
         :<|> updateTweetHandler acid
         :<|> deleteTweetHandler acid

-- Обработчики для аутентификации
registerHandler :: AcidState AppState -> (Text, Text) -> Handler NoContent
registerHandler acid (username, password) = do
  unless (isValidUsername username) $
    throwError err400 { errBody = "Invalid username format" }

  let passLength = T.length password
  unless (passLength > 3 && passLength < 18) $
    throwError err400 { errBody = "Password must be between 4 and 17 characters" }

  mExistingUser <- liftIO $ query' acid (GetUser username)
  case mExistingUser of
    Just _ -> throwError err400 { errBody = "Username already exists" }
    Nothing -> do
      let user = User username password
      liftIO $ update' acid (AddUser user)
      return NoContent

loginHandler :: AcidState AppState -> (Text, Text) -> Handler AuthToken
loginHandler acid (inputUsername, inputPassword) = do
  mUser <- liftIO $ query' acid (GetUser inputUsername)
  case mUser of
    Nothing -> throwError err401 { errBody = "Invalid username or password" }
    Just user ->
      if validatePassword' inputPassword (password user)
      then do
        allTokens <- liftIO $ query' acid GetAllTokens
        let userTokens = Map.filter (\t -> tokenUser t == inputUsername) allTokens
        liftIO $ mapM_ (update' acid . RemoveAuthToken . tokenId) (Map.elems userTokens)

        token <- liftIO $ generateToken inputUsername
        liftIO $ update' acid (AddAuthToken token)
        return token
      else throwError err401 { errBody = "Invalid username or password" }

logoutHandler :: AcidState AppState -> Maybe Text -> Handler NoContent
logoutHandler acid Nothing = throwError err401 { errBody = "Authorization token required" }
logoutHandler acid (Just tokenId) = do
  liftIO $ update' acid (RemoveAuthToken tokenId)
  return NoContent

-- Обновленные обработчики твитов с аутентификацией
authenticate :: AcidState AppState -> Maybe Text -> Handler Text
authenticate acid Nothing = throwError err401 { errBody = "Authorization token required" }
authenticate acid (Just tokenId) = do
  mToken <- liftIO $ query' acid (GetAuthToken tokenId)
  case mToken of
    Nothing -> throwError err401 { errBody = "Invalid token" }
    Just token -> do
      isValid <- liftIO $ isTokenValid token
      if isValid
        then return $ tokenUser token
        else do
          liftIO $ update' acid (RemoveAuthToken tokenId)
          throwError err401 { errBody = "Token expired" }

sendTweetHandler :: AcidState AppState -> Maybe Text -> Text -> Handler NoContent
sendTweetHandler acid mToken tweetContent = do
  username <- authenticate acid mToken

  when (T.length tweetContent > 280) $
    throwError err400 { errBody = "Tweet is too long (max 280 characters)" }

  currentTime <- liftIO getCurrentTime
  nextId <- liftIO $ query' acid GetNextTweetId
  let newTweet = Tweet nextId username tweetContent currentTime
  liftIO $ do
    update' acid (AddTweet newTweet)
    update' acid IncrementTweetId
  return NoContent

searchTweetsHandler :: AcidState AppState -> [Text] -> [Text] -> [Text] -> Handler [Tweet]
searchTweetsHandler acid tags fromUsers mentions = do
  let tagsList = concatMap (T.splitOn ",") tags
      fromList = concatMap (T.splitOn ",") fromUsers
      mentionsList = concatMap (T.splitOn ",") mentions

  when (null tagsList && null fromList && null mentionsList) $ do
    throwError err400 { errBody = "At least one search parameter is required: tags or mentions or from" }

  case filter (not . isValidUsername) fromList of
    [] -> return ()
    _  -> throwError err400 { errBody = "Invalid username format in 'from'" }

  case filter (not . isValidUsername) mentionsList of
    [] -> return ()
    _ -> throwError err400 { errBody = "Invalid username format in 'mentions'" }

  case filter (not . isValidTag) tagsList of
    [] -> return ()
    _  -> throwError err400 { errBody = "Invalid tag format" }

  allTweets <- liftIO $ query' acid GetTweets
  return $ filter (matchesCriteria tagsList fromList mentionsList) allTweets

updateTweetHandler :: AcidState AppState -> Int -> Maybe Text -> Text -> Handler NoContent
updateTweetHandler acid tweetId mToken newContent = do
  username <- authenticate acid mToken
  success <- liftIO $ update' acid (UpdateTweet tweetId newContent username)
  if success
    then return NoContent
    else throwError err404 { errBody = "Tweet not found or you don't have permission to edit it" }

deleteTweetHandler :: AcidState AppState -> Int -> Maybe Text -> Handler NoContent
deleteTweetHandler acid tweetId mToken = do
  username <- authenticate acid mToken
  success <- liftIO $ update' acid (DeleteTweet tweetId username)
  if success
    then return NoContent
    else throwError err404 { errBody = "Tweet not found or you don't have permission to delete it" }

-- Вспомогательные функции
isValidUsername :: T.Text -> Bool
isValidUsername name = (T.unpack name :: String) =~ ("^[a-zA-Z][a-zA-Z0-9]*$" :: String)

isValidTag :: Text -> Bool
isValidTag = isValidUsername

matchesCriteria :: [Text] -> [Text] -> [Text] -> Tweet -> Bool
matchesCriteria tags fromUsers mentions tweet =
  matchesTags tags tweet && matchesFrom fromUsers tweet && matchesMentions mentions tweet

matchesTags :: [Text] -> Tweet -> Bool
matchesTags [] _     = True
matchesTags ts tweet = all (\tag -> any (== tag) (T.words $ content tweet)) (map ("#" <>) ts)

matchesFrom :: [Text] -> Tweet -> Bool
matchesFrom [] _        = True
matchesFrom users tweet = author tweet `elem` users

matchesMentions :: [Text] -> Tweet -> Bool
matchesMentions [] _ = True
matchesMentions users tweet = all (\mention -> any (== mention) (T.words $ content tweet)) (map ("@" <>) users)

rejectUnknownParams :: [ByteString] -> Middleware
rejectUnknownParams allowedParams app req sendResponse =
  let params = map fst (queryString req)
      unknownParams = filter (`notElem` allowedParams) params
  in if null unknownParams
     then app req sendResponse
     else sendResponse $ responseLBS status400 [] "Unknown query parameters found"
