{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

import           Control.Monad            (when)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader     (ask)
import           Control.Monad.State      (get, put)
import           Data.Acid
import           Data.Acid.Advanced       (query', update')
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BS
import           Data.List                (find, isInfixOf)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe               (fromMaybe, isNothing)
import           Data.SafeCopy            (SafeCopy, base, deriveSafeCopy)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Data.Time.Clock          (UTCTime, addUTCTime, getCurrentTime)
import           Data.Time.Format         (defaultTimeLocale, formatTime)
import           GHC.Generics             (Generic)
import           Network.HTTP.Types       (status400, status401, status403,
                                           status404)
import           Network.Wai
import           Network.Wai              (queryString, responseLBS)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.Server           (err400, err401, err403, err404)
import           System.Random            (randomIO)
import           Text.Regex.TDFA          ((=~))

data Tweet = Tweet
  { tweetId   :: Int  -- Добавляем ID для идентификации твитов
  , author    :: Text
  , content   :: Text
  , timestamp :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON Tweet
instance FromJSON Tweet

data TweetsState = TweetsState
  { tweets      :: [Tweet]
  , nextTweetId :: Int  -- Счетчик для генерации ID
  } deriving (Show)

initialState :: TweetsState
initialState = TweetsState [] 1

-- Acid-state операции

addTweet :: Tweet -> Update TweetsState ()
addTweet tweet = do
  s <- get
  put $ s { tweets = tweet : tweets s }

getTweets :: Query TweetsState [Tweet]
getTweets = do
  s <- ask
  return $ tweets s

getNextTweetId :: Query TweetsState Int
getNextTweetId = do
  s <- ask
  return $ nextTweetId s

incrementTweetId :: Update TweetsState ()
incrementTweetId = do
  s <- get
  put $ s { nextTweetId = nextTweetId s + 1 }

updateTweet :: Int -> Text -> Text -> Update TweetsState Bool
updateTweet idToUpdate newContent requestingUser = do
  s <- get
  case find (\t -> tweetId t == idToUpdate && author t == requestingUser) (tweets s) of
    Just oldTweet -> do
      let updatedTweet = oldTweet { content = newContent }
          newTweets = updatedTweet : filter (\t -> tweetId t /= idToUpdate) (tweets s)
      put $ s { tweets = newTweets }
      return True
    Nothing -> return False

deleteTweet :: Int -> Text -> Update TweetsState Bool
deleteTweet idToDelete requestingUser = do
  s <- get
  case find (\t -> tweetId t == idToDelete && author t == requestingUser) (tweets s) of
    Just _ -> do
      let newTweets = filter (\t -> tweetId t /= idToDelete) (tweets s)
      put $ s { tweets = newTweets }
      return True
    Nothing -> return False

$(deriveSafeCopy 0 'base ''Tweet)
$(deriveSafeCopy 0 'base ''TweetsState)
$(makeAcidic ''TweetsState ['addTweet, 'getTweets, 'getNextTweetId, 'incrementTweetId, 'updateTweet, 'deleteTweet])

-- API определение

type API =
       "send" :> QueryParam "as" Text :> ReqBody '[PlainText] Text :> Post '[JSON] NoContent
  :<|> "search" :> QueryParams "tags" Text :> QueryParams "from" Text :> QueryParams "mentions" Text :> Get '[JSON] [Tweet]
  :<|> "tweets" :> Capture "tweetId" Int :> ReqBody '[PlainText] Text :> QueryParam "as" Text :> Put '[JSON] NoContent
  :<|> "tweets" :> Capture "tweetId" Int :> QueryParam "as" Text :> Delete '[JSON] NoContent

server :: AcidState TweetsState -> Server API
server acid = sendTweet acid
          :<|> searchTweets acid
          :<|> updateTweetHandler acid
          :<|> deleteTweetHandler acid

-- Обработчики

-- POST /send?as=user
sendTweet :: AcidState TweetsState -> Maybe Text -> Text -> Handler NoContent
sendTweet acid user tweetContent = do
  liftIO $ putStrLn $ "Received request: user=" ++ show user ++ ", content=" ++ T.unpack tweetContent
  case user of
    Just u | not (isValidUsername u) -> throwError err400 { errBody = "Invalid username format" }
    Just u | T.length tweetContent > 280 -> throwError err400 { errBody = "Tweet is too long (max 280 characters)" }
    Just u -> do
      currentTime <- liftIO getCurrentTime
      nextId <- liftIO $ query' acid GetNextTweetId
      let newTweet = Tweet nextId u tweetContent currentTime
      liftIO $ do
        putStrLn $ "Adding tweet: " ++ show newTweet
        update' acid (AddTweet newTweet)
        update' acid IncrementTweetId
      return NoContent
    Nothing -> throwError err400 { errBody = "User must be specified" }

-- GET /search
searchTweets :: AcidState TweetsState -> [Text] -> [Text] -> [Text] -> Handler [Tweet]
searchTweets acid tags fromUsers mentions = do
  liftIO $ putStrLn $ "[INFO] Received GET /search request: tags=" ++ show tags ++ ", from=" ++ show fromUsers ++ ", mentions=" ++ show mentions

  let tagsList = concatMap (T.splitOn ",") tags
      fromList = concatMap (T.splitOn ",") fromUsers
      mentionsList = concatMap (T.splitOn ",") mentions

  when (null tagsList && null fromList && null mentionsList) $ do
    throwError err400 { errBody = "At least one search parameter is required: tags or mentions or from" }

  case filter (not . isValidUsername) fromList of
    [] -> return ()
    _ -> do
      throwError err400 { errBody = "Invalid username format in 'from'" }

  case filter (not . isValidUsername) mentionsList of
    [] -> return ()
    _ -> do
      throwError err400 { errBody = "Invalid username format in 'mentions'" }

  case filter (not . isValidTag) tagsList of
    [] -> return ()
    _ -> do
      throwError err400 { errBody = "Invalid tag format" }

  allTweets <- liftIO $ query' acid GetTweets
  liftIO $ putStrLn $ "[INFO] Found " ++ show (length allTweets) ++ " tweets"
  return $ filter (matchesCriteria tagsList fromList mentionsList) allTweets

-- PUT /tweets/:id?as=user
updateTweetHandler :: AcidState TweetsState -> Int -> Text -> Maybe Text -> Handler NoContent
updateTweetHandler acid tweetId newContent user = do
  case user of
    Just u -> do
      success <- liftIO $ update' acid (UpdateTweet tweetId newContent u)
      if success
        then return NoContent
        else throwError err404 { errBody = "Tweet not found or you don't have permission to edit it" }
    Nothing -> throwError err400 { errBody = "User must be specified" }

-- DELETE /tweets/:id?as=user
deleteTweetHandler :: AcidState TweetsState -> Int -> Maybe Text -> Handler NoContent
deleteTweetHandler acid tweetId user = do
  case user of
    Just u -> do
      success <- liftIO $ update' acid (DeleteTweet tweetId u)
      if success
        then return NoContent
        else throwError err404 { errBody = "Tweet not found or you don't have permission to delete it" }
    Nothing -> throwError err400 { errBody = "User must be specified" }

-- Вспомогательные функции

isValidUsername :: T.Text -> Bool
isValidUsername name = (T.unpack name :: String) =~ ("^[a-zA-Z][a-zA-Z0-9]*$" :: String)

isValidTag :: Text -> Bool
isValidTag tag = isValidUsername tag

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

main :: IO ()
main = do
  acid <- openLocalState initialState
  putStrLn "Server running at http://localhost:8080"
  run 8080 $ rejectUnknownParams ["from", "mentions", "as", "tags"] (serve (Proxy :: Proxy API) (server acid))
