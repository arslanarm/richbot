{-# LANGUAGE OverloadedStrings #-}
module Main where
import Telegram.Bot.Simple.BotApp (BotApp(BotApp, botInitialModel, botAction, botHandler), startBot_)
import Telegram.Bot.API (Update (updateInlineQuery, updateMessage), InlineQueryId, defAnswerInlineQuery, Token (Token), defaultTelegramClientEnv, InlineQuery (inlineQueryId, inlineQueryQuery), Message (messageText))
import qualified Data.Text as Text
import Data.Text (Text, splitOn, unpack, pack)
import Telegram.Bot.API.InlineMode.InlineQueryResult (InlineQueryResultGeneric(inlineQueryResultTitle, inlineQueryResultInputMessageContent), defInlineQueryResultGeneric, InlineQueryResultId (InlineQueryResultId), defInlineQueryResultGenericThumbnail, defInlineQueryResultArticle)
import Telegram.Bot.API.InlineMode.InputMessageContent (defaultInputTextMessageContent)
import Telegram.Bot.Simple
    ( BotApp(..), Eff, startBot_, (<#), RunTG(runTG), replyText )
import Data.Maybe (isJust)
import Telegram.Bot.Simple.UpdateParser (updateMessageText)
import Database.Selda ( Text, MonadIO (liftIO) )
import Database.Selda.PostgreSQL ( withPostgreSQL, seldaClose, pgOpen, PG )
import Database (getAllMachines, VendingMachine (machineName, mid), connectionConfiguration, createAllTables, insertMachine)
import VendingMachineApi (getToken, getItems, authorizeToken, VendingMachineItem (name), VendingMachineData (machine), VendingMachineInfo (planogram))
import qualified Control.Monad
import Control.Concurrent ( threadDelay, forkIO )
import Control.Monad ( forever, forM_ )
import VendingMachineManager ( save )
import Text.Printf (printf)
import Control.Exception ( bracket )
import Database.Selda.Backend (SeldaConnection, runSeldaT)
import GHC.Base (IO(..))
import System.Environment (getEnv)

main :: IO ()
main = do
  -- withPostgreSQL connectionConfiguration $ do
  --   _ <- createAllTables
  --   return ()
  username <- getEnv "USERNAME"
  password <- getEnv "PASSWORD"
  devid <- getEnv "DEVID"
  address <- getEnv "ADDRESS"
  forkIO $ forever $ do
    token <- getToken username password devid address
    withPostgreSQL connectionConfiguration $ do
      vendingMachines <- getAllMachines
      forM_ vendingMachines $ \vendingMachine -> do
        liftIO $ printf "Checking %s\n" (machineName vendingMachine)
        items <- liftIO $ getItems (authorizeToken token) (Text.unpack $ machineName vendingMachine) address
        liftIO $ printf "Got %d items\n" (length (planogram (machine items)))
        save (machineName vendingMachine) items
    threadDelay 60000000  -- Delay for one minute

  putStrLn "Please enter the token"
  token <- Token . Text.pack <$> getLine
  run token

data Model = Model (SeldaConnection PG)

data Action
  = AddMachine Text
  | GetMachines
  | GetItems
  | GetMachineItems Int

bot :: Model -> BotApp Model Action
bot connection = BotApp
  { botInitialModel = connection
  , botAction = updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ = case updateMessage update of
    Just msg -> do
      text <- messageText msg
      let splitted = splitOn " " text
      let args = tail splitted
      case head splitted of
        "/add_machine" -> Just $ AddMachine (head args)
        "/get_machines" -> Just GetMachines
        "/get_items" -> Just GetItems
        "/get_machine_items" -> Just $ GetMachineItems (read $ unpack $ head args)
        _ -> Nothing
    _ -> Nothing


handleAction :: Action -> Model -> Eff Action Model
handleAction action model@(Model connection) = case action of
  AddMachine name -> model <# do
    result <- liftIO $ runSeldaT (insertMachine name) connection

    replyText ("Successfully added machine: " <> name)
  GetMachines -> model <# do
    result <- liftIO $ runSeldaT getAllMachines connection
  
    replyText (foldr "" (++ " ") (map machineToString result))
  GetItems -> model <# do
    pure ""
  GetMachineItems machineId -> model <# do
    pure ""

machineToString :: VendingMachine -> Text
machineToString machine = "ID: " <> pack (show $ mid machine) <> " Name: " <> machineName machine


run :: Token -> IO ()
run token = bracket (pgOpen connectionConfiguration) seldaClose $ \connection -> do
    env <- defaultTelegramClientEnv token
    startBot_ (bot (Model connection)) env
