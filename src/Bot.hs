{-# LANGUAGE OverloadedStrings #-}

module Bot where

--Custom file to keep log paths
import LogFilePaths

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Format
import Options.Applicative

--Types needed from Reddit API
import Reddit
import Reddit.Types.Post
import Reddit.Types.Subreddit
import Reddit.Types.User
import Reddit.Types.Message
import Reddit.Types.Listing
import Reddit.Actions.Message
import Reddit.Actions.Thing

--Needed to be able to exit posting thread
import System.Exit
import Text.Read
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Default.Class

import System.Random
import Data.List.Split
import Data.Maybe


--Main entry for bot
main :: IO ()
main = do 
    execParser (info (helper <*> argsParser) fullDesc) >>= runBot

--Data and Types
--Fact Data
data FactType = General |  Cardinals | Falcons | Ravens | Bills | Panthers
                | Bears | Bengals | Browns | Cowboys | Broncos | Lions | Packers
                | Texans | Colts | Jaguars | Chiefs | Chargers | Rams | Dolphins 
                | Vikings | Patriots | Saints | Giants | Jets | Raiders | Eagles 
                | Steelers | Niners | Seahawks | Buccaneers | Titans | Redskins   
    deriving (Eq, Ord, Show, Read, Enum)

data NflFact = NflFact {
    key :: Int,
    fact :: Text
} deriving Show

data BotRedditSettings = BotRedditSettings {
    bans :: [Username]
} deriving (Show)

--Arguments needed to be able to have Reddit successfully do necessary commands
--Includes Username and Pass for logging in, Subreddit Name for where to post, and a log file for post content
data Args = 
    Args { 
        username :: Username, 
        password :: Text,        
        logFileName :: Maybe FilePath }
    deriving (Show, Read, Eq)

--argumentParser is needed to parse the arguments from a log file and make them Reddit friendly
argsParser :: Parser Args 
argsParser = Args <$> (Username <$> argument text (metavar "USERNAME"))
                  <*> argument text (metavar "PASSWORD")                  
                  <*> optional (option str (long "log-file" <> metavar "LOG"))
    where text = fmap Text.pack str

--Run bot starts the bot process and is where most of the executable code is
runBot :: Args -> IO ()
runBot a@(Args userName passWord _log) = do
    putStrLn "RUNNING BOT VERSION 0.3.3.3"
    writeToLogFile "RUNNING BOT VERSION 0.3.3.3"
    putStrLn "+++++ STARTING BOT THREAD +++++"    
    writeToLogFile "+++++ STARTING BOT THREAD +++++"

    --Load in Fact lists
    allFacts <- getAllFacts
    putStrLn "++ Loaded in facts ++"
    writeToLogFile "++ Loaded in facts ++"
    --Load in reply 
    replyText <- fileLogReader inputFile
    putStrLn "++ Loaded in input log ++"
    writeToLogFile "++ Loaded in input log ++"

    --Defining the login options necessary for Reddit Account login
    let opts = def { loginMethod = Credentials u passWord,
        customUserAgent = Just "NFL_FUNFACT_BOT v0.3.3.x" }

    response <- runRedditWith opts $ do

        let textShow = Text.pack . show

        --Retrieves unread messages from inbox
        Listing _ _ messages  <- getUnreadMessages
        if null messages
            then liftIO $ Text.putStrLn "-----No unread messages at the moment-----"
                else forM_ messages $ \message -> do
                        liftIO $ Text.putStrLn $ 
                            "** Receieved Message: [" <> textShow (messageID message) <> "] " <>
                            subject message <> ": " <> body message <> "... "
                        liftIO $ writeToLogFile $ (Text.unpack) ("** Receieved Message: [" <> textShow (messageID message) <> "] : " <> body message <> "... ")

        if null messages 
            then liftIO $ Text.putStrLn "-----Nothing to reply to-----"
                else forM_ messages $ \replyMess -> do                    
                    if (isMentioned userName (body replyMess)) 
                        then if isJust (containsTeam (body replyMess))
                                then do teamReply <- liftIO $ buildReplyText (teamFactReply allFacts (body replyMess)) replyText
                                        resp <- reply (messageID replyMess) teamReply
                                        liftIO $ Text.putStrLn "*** Found team related mention ***"
                                        liftIO $ writeToLogFile "*** Found team related mention ***"
                                        liftIO $ Text.putStrLn $ "***Team Reply Comment ID: " <> (Text.pack . show ) resp <> "..."
                                        liftIO $ writeToLogFile $ (Text.unpack) ("***Team Reply Comment ID: " <> (Text.pack . show ) resp <> "...")
                                    else do fact <- liftIO $ buildReplyText (randomFactReply allFacts) replyText
                                            res <- replyWithText fact (messageID replyMess)                                        
                                            liftIO $ Text.putStrLn $ "***Replied to comment: " <> textShow (messageID replyMess) <> "..."
                                            liftIO $ writeToLogFile $ (Text.unpack) ("***Replied to comment: " <> textShow (messageID replyMess) <> "...")
                                            liftIO $ Text.putStrLn $ "***Reply Comment ID: " <> (Text.pack . show ) res <> "..."
                                            liftIO $ writeToLogFile $ (Text.unpack) ("***Reply Comment ID: " <> (Text.pack . show ) res <> "...")
                                            
                            else liftIO $ Text.putStrLn "-----Comment is not a mention-----"

        
        --Marks the messages as read that were retrieved so that they dont appear again.
        if null messages
            then liftIO $ Text.putStrLn "-----Inbox empty-----"
                else forM_ messages $ \readMessage -> do
                        liftIO $ Text.putStrLn $ "**Message Marked as read: " <> textShow (messageID readMessage) <> "..."
                        res <- markRead (messageID readMessage)
                        return res
        
    --Determines what to do if Success/Failure
    case response of
        Left err -> do 
            print err
            putStrLn "--- Reddit Run Failure ---"
            writeToLogFile "--- Reddit Run Failure ---"
            --Delaying thread for 5 mins. In case of a network or Reddit maintenance issue so it wont ping Reddit's servers too much.
            threadDelay (300 * 1000000)
            --exitFailure
            runBot a
        Right p -> do
            putStrLn "*** Reddit Run Success ***" 
            writeToLogFile "*** Reddit Run Success ***"
            --Delaying thread for 20 seconds
            threadDelay (20 * 1000000)
            runBot a
    where Username u = userName

--Checks for Reddit API errors
httpCheck :: MonadIO m => RedditT m a -> RedditT m a
httpCheck a = do 
  r <- nest a 
  case r of 
    Left (HTTPError _) -> do 
      liftIO $ threadDelay $ 5 * 1000 * 1000
      httpCheck a
    Left e -> failWith e
    Right x -> return x 

getAllFacts :: IO [(Int,Text)]
getAllFacts = do
    f0 <- fileReader genFactFile
    f1 <- fileReader cardinalsFactFile
    f2 <- fileReader falconsFactFile
    f3 <- fileReader ravensFactFile
    f4 <- fileReader billsFactFile
    f5 <- fileReader panthersFactFile
    f6 <- fileReader bearsFactFile
    f7 <- fileReader bengalsFactFile
    f8 <- fileReader brownsFactFile
    f9 <- fileReader cowboysFactFile
    f10 <- fileReader broncosFactFile
    f11 <- fileReader lionsFactFile
    f12 <- fileReader packersFactFile
    f13 <- fileReader texansFactFile
    f14 <- fileReader coltsFactFile
    f15 <- fileReader jaguarsFactFile
    f16 <- fileReader chiefsFactFile
    f17 <- fileReader chargersFactFile
    f18 <- fileReader ramsFactFile
    f19 <- fileReader dolphinsFactFile
    f20 <- fileReader vikingsFactFile
    f21 <- fileReader patriotsFactFile
    f22 <- fileReader saintsFactFile
    f23 <- fileReader giantsFactFile
    f24 <- fileReader jetsFactFile
    f25 <- fileReader raidersFactFile
    f26 <- fileReader eaglesFactFile
    f27 <- fileReader steelersFactFile
    f28 <- fileReader ninersFactFile
    f29 <- fileReader seahawksFactFile
    f30 <- fileReader buccaneersFactFile
    f31 <- fileReader titansFactFile
    f32 <- fileReader redskinsFactFile
    let firstFacts = f0 ++ f1 ++ f2 ++ f3 ++ f4 ++ f5 ++ f6 ++ f7 ++ f8 ++ f9 ++ f10
    let secondFacts = f11 ++ f12 ++ f13 ++ f14 ++ f15 ++ f16 ++ f17 ++ f18 ++ f19 ++ f20 ++ f21
    let thirdFacts = f22 ++ f23 ++ f24 ++ f25 ++ f26 ++ f27 ++ f28 ++ f29 ++ f30 ++ f31 ++ f32
    let allFacts = firstFacts ++ secondFacts ++ thirdFacts
    putStrLn "[getAllFacts]: Getting all facts"
    return allFacts

buildReplyText :: IO Text -> Text -> IO Text
buildReplyText ioTxt1 txt2 = do
    txt1 <- ioTxt1
    let combText = txt1 <> txt2
    writeToLogFile $ "[buildReplyText]: { " <> (Text.unpack) combText <> " } "
    return combText 

writeToLogFile :: String -> IO ()
writeToLogFile  text = do 
    time <- liftIO getCurrentTime
    let filePath = "logFile.md"
    appendFile filePath $ concat [show (timeToText time), " ", "LOG INFO: ", text, "\n"]

timeToText :: UTCTime -> Text
timeToText time = Text.pack $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) time

----------------
--Reddit Section
----------------
getUnreadMessages :: RedditT IO (Listing MessageKind Message)
getUnreadMessages = do    
    res <- getUnread
    return res

isMentioned :: Username -> Text -> Bool
isMentioned (Username userName) = Text.isInfixOf (Text.toCaseFold $ "u/" <> userName) . Text.toCaseFold

containsTeam :: Text -> Maybe FactType
containsTeam body | Text.isInfixOf "Cardinals" body  == True = Just Cardinals
                  | Text.isInfixOf "Falcons" body  == True = Just Falcons
                  | Text.isInfixOf "Ravens" body  == True = Just Ravens
                  | Text.isInfixOf "Bills" body  == True = Just Bills
                  | Text.isInfixOf "Panthers" body  == True = Just Panthers
                  | Text.isInfixOf "Bears" body  == True = Just Bears
                  | Text.isInfixOf "Bengals" body  == True = Just Bengals
                  | Text.isInfixOf "Browns" body  == True = Just Browns
                  | Text.isInfixOf "Cowboys" body  == True = Just Cowboys
                  | Text.isInfixOf "Broncos" body  == True = Just Broncos
                  | Text.isInfixOf "Lions" body  == True = Just Lions
                  | Text.isInfixOf "Packers" body  == True = Just Packers
                  | Text.isInfixOf "Texans" body  == True = Just Texans
                  | Text.isInfixOf "Colts" body  == True = Just Colts
                  | Text.isInfixOf "Jaguars" body  == True = Just Jaguars
                  | Text.isInfixOf "Chiefs" body  == True = Just Chiefs
                  | Text.isInfixOf "Chargers" body  == True = Just Chargers
                  | Text.isInfixOf "Rams" body  == True = Just Rams
                  | Text.isInfixOf "Dolphins" body  == True = Just Dolphins
                  | Text.isInfixOf "Vikings" body  == True = Just Vikings
                  | Text.isInfixOf "Patriots" body  == True = Just Patriots
                  | Text.isInfixOf "Saints" body  == True = Just Saints
                  | Text.isInfixOf "Giants" body  == True = Just Giants
                  | Text.isInfixOf "Jets" body  == True = Just Jets
                  | Text.isInfixOf "Raiders" body  == True = Just Raiders
                  | Text.isInfixOf "Eagles" body  == True = Just Eagles
                  | Text.isInfixOf "Steelers" body  == True = Just Steelers
                  | Text.isInfixOf "Niners" body == True || Text.isInfixOf "49ers" body == True = Just Niners
                  | Text.isInfixOf "Seahawks" body  == True = Just Seahawks
                  | Text.isInfixOf "Buccaneers" body  == True = Just Buccaneers
                  | Text.isInfixOf "Titans" body  == True = Just Titans
                  | Text.isInfixOf "Redskins" body  == True = Just Redskins
                  | Text.isInfixOf "general" body == True = Just General
                  | otherwise = Nothing
 

------------------
--Custom Actions
------------------
replyWithText :: Monad m => Text -> MessageKind -> RedditT m CommentID
replyWithText txt mess = do 
    res <- reply mess txt
    return res

randomFactReply :: [(Int,Text)] -> IO (Text)
randomFactReply assoc = do 
    maybeRandomFact <- getRandomFactAssoc assoc
    if isJust maybeRandomFact
        then do putStrLn "[randomFactReply]: Got random fact"
                return $ (Text.pack) "**Here is a random NFL fact:**  " <> (fromJust maybeRandomFact)          
            else do putStrLn "----[randomFactReply]: Problem getting random fact ----"
                    return ((Text.pack) "[randomFactReply]: There was a problem getting the random fact reply...")

teamFactReply :: [(Int,Text)] -> Text -> IO (Text)
teamFactReply assoc txt = do
    let maybeFactType = containsTeam txt
    if isJust maybeFactType
        then do maybeTeamFact <- getTeamFact assoc (fromJust maybeFactType)
                if isJust maybeTeamFact
                    then do putStrLn "[teamFactReply]: Got team fact"
                            return $ (Text.pack)"**Here is your " <> (Text.pack . show) (fromJust maybeFactType) <> (Text.pack)" fact:**  " <> fromJust maybeTeamFact
                        else do putStrLn "----[teamFactReply]: Problem getting team fact ----"
                                return ((Text.pack) "----[teamFactReply]There was a problem getting the team fact reply ----")
            else do putStrLn "----[teamFactReply]: Problem getting team type. Just getting a random fact instead. This block should never be seen ---- "
                    writeToLogFile $ (Text.unpack) "----[teamFactReply]: Problem getting team type. Just getting a random fact instead. This block should never be seen ---- "
                    randomReply <- randomFactReply assoc
                    return randomReply
            
--Randomizer Section
---Random Int Gen
randomInt :: IO Int
randomInt = getStdRandom (randomR (00000, 32999))

getRandomFactAssoc :: [(Int,Text)] -> IO (Maybe Text)
getRandomFactAssoc assoc = do
    key <- getRandomKey 
    let fact = lookupFact key assoc
    return fact

getTeamFact :: [(Int,Text)] -> FactType -> IO (Maybe Text)
getTeamFact assoc factType = do
    key <- getTeamKey factType
    let fact = lookupTeamFact key assoc
    return fact

getTeamKey :: FactType -> IO Int
getTeamKey factType = do 
    let filePath = determineFilePath' factType
    let keyPrefix = getTeamKeyPrefix factType
    h <- getFactLength filePath    
    factNum <- randomRIO(000, h - 1)
    let combNum = (keyPrefix * 1000) + factNum
    return combNum

getRandomKey :: IO Int
getRandomKey = do
    typeNum <- randomRIO(00, (fromEnum Redskins))
    let filePath = determineFilePath typeNum
    let typeNum2 = typeNum * 1000
    h <- getFactLength filePath
    factNum <- randomRIO(000, h - 1)
    let combNum = typeNum2 + factNum
    return combNum

getTeamKeyPrefix :: FactType -> Int
getTeamKeyPrefix factType | factType == Cardinals = fromEnum Cardinals 
                          | factType == Falcons = fromEnum Falcons
                          | factType == Ravens = fromEnum Ravens
                          | factType == Bills = fromEnum Bills
                          | factType == Panthers = fromEnum Panthers
                          | factType == Bears = fromEnum Bears
                          | factType == Bengals = fromEnum Bengals
                          | factType == Browns = fromEnum Browns
                          | factType == Cowboys = fromEnum Cowboys
                          | factType == Broncos = fromEnum Broncos
                          | factType == Lions = fromEnum Lions
                          | factType == Packers = fromEnum Packers
                          | factType == Texans = fromEnum Texans
                          | factType == Colts = fromEnum Colts
                          | factType == Jaguars = fromEnum Jaguars
                          | factType == Chiefs = fromEnum Chiefs
                          | factType == Chargers = fromEnum Chargers
                          | factType == Rams = fromEnum Rams
                          | factType == Dolphins = fromEnum Dolphins
                          | factType == Vikings = fromEnum Vikings
                          | factType == Patriots = fromEnum Patriots
                          | factType == Saints = fromEnum Saints
                          | factType == Giants = fromEnum Giants
                          | factType == Jets = fromEnum Jets
                          | factType == Raiders = fromEnum Raiders
                          | factType == Eagles = fromEnum Eagles
                          | factType == Steelers = fromEnum Steelers
                          | factType == Niners = fromEnum Niners
                          | factType == Seahawks = fromEnum Seahawks
                          | factType == Buccaneers = fromEnum Buccaneers
                          | factType == Titans = fromEnum Titans
                          | factType == Redskins = fromEnum Redskins
                          | otherwise = 00    

determineFilePath :: Int -> FilePath
determineFilePath x | x == (fromEnum Cardinals) = (determineFilePath' Cardinals)
                    | x == (fromEnum Falcons) = (determineFilePath' Falcons)
                    | x == (fromEnum Ravens) = (determineFilePath' Ravens)
                    | x == (fromEnum Bills) = (determineFilePath' Bills)
                    | x == (fromEnum Panthers) = (determineFilePath' Panthers)
                    | x == (fromEnum Bears) = (determineFilePath' Bears)
                    | x == (fromEnum Bengals) = (determineFilePath' Bengals)
                    | x == (fromEnum Browns) = (determineFilePath' Browns)
                    | x == (fromEnum Cowboys) = (determineFilePath' Cowboys)
                    | x == (fromEnum Broncos) = (determineFilePath' Broncos)
                    | x == (fromEnum Lions) = (determineFilePath' Lions)
                    | x == (fromEnum Packers) = (determineFilePath' Packers)
                    | x == (fromEnum Texans) = (determineFilePath' Texans)
                    | x == (fromEnum Colts) = (determineFilePath' Colts)
                    | x == (fromEnum Jaguars) = (determineFilePath' Jaguars)
                    | x == (fromEnum Chiefs) = (determineFilePath' Chiefs)
                    | x == (fromEnum Chargers) = (determineFilePath' Chargers)
                    | x == (fromEnum Rams) = (determineFilePath' Rams)
                    | x == (fromEnum Dolphins) = (determineFilePath' Dolphins)
                    | x == (fromEnum Vikings) = (determineFilePath' Vikings)
                    | x == (fromEnum Patriots) = (determineFilePath' Patriots)
                    | x == (fromEnum Saints) = (determineFilePath' Saints)
                    | x == (fromEnum Giants) = (determineFilePath' Giants)
                    | x == (fromEnum Jets) = (determineFilePath' Jets)
                    | x == (fromEnum Raiders) = (determineFilePath' Raiders)
                    | x == (fromEnum Eagles) = (determineFilePath' Eagles)
                    | x == (fromEnum Steelers) = (determineFilePath' Steelers)
                    | x == (fromEnum Niners) = (determineFilePath' Niners)
                    | x == (fromEnum Seahawks) = (determineFilePath' Seahawks)
                    | x == (fromEnum Buccaneers) = (determineFilePath' Buccaneers)
                    | x == (fromEnum Titans) = (determineFilePath' Titans)
                    | x == (fromEnum Redskins) = (determineFilePath' Redskins)
                    | otherwise = genFactFile

determineFilePath' :: FactType -> FilePath
determineFilePath' factType | factType == Cardinals = cardinalsFactFile
                            | factType == Falcons = falconsFactFile
                            | factType == Ravens = ravensFactFile
                            | factType == Bills = billsFactFile
                            | factType == Panthers = panthersFactFile
                            | factType == Bears = bearsFactFile
                            | factType == Bengals = bengalsFactFile
                            | factType == Browns = brownsFactFile
                            | factType == Cowboys = cowboysFactFile
                            | factType == Broncos = broncosFactFile
                            | factType == Lions = lionsFactFile
                            | factType == Packers = packersFactFile
                            | factType == Texans = texansFactFile
                            | factType == Colts = coltsFactFile
                            | factType == Jaguars = jaguarsFactFile
                            | factType == Chiefs = chiefsFactFile
                            | factType == Chargers = chargersFactFile
                            | factType == Rams = ramsFactFile
                            | factType == Dolphins = dolphinsFactFile
                            | factType == Vikings = vikingsFactFile
                            | factType == Patriots = patriotsFactFile
                            | factType == Saints = saintsFactFile
                            | factType == Giants = giantsFactFile
                            | factType == Jets = jetsFactFile
                            | factType == Raiders = raidersFactFile
                            | factType == Eagles = eaglesFactFile
                            | factType == Steelers = steelersFactFile
                            | factType == Niners = ninersFactFile
                            | factType == Seahawks = seahawksFactFile
                            | factType == Buccaneers = buccaneersFactFile
                            | factType == Titans = titansFactFile
                            | factType == Redskins = redskinsFactFile
                            | otherwise = genFactFile

determineTeamFromKey :: Int -> FactType
determineTeamFromKey x | x >= (fromEnum Cardinals) * 1000 && x < (fromEnum Falcons) * 1000 = Cardinals 
                       | x >= (fromEnum Falcons) * 1000 && x < (fromEnum Ravens) * 1000 = Falcons
                       | x >= (fromEnum Ravens) * 1000 && x < (fromEnum Bills) * 1000 = Ravens
                       | x >= (fromEnum Bills) * 1000 && x < (fromEnum Panthers) * 1000 = Bills
                       | x >= (fromEnum Panthers) * 1000 && x < (fromEnum Bears) * 1000 = Panthers
                       | x >= (fromEnum Bears) * 1000 && x < (fromEnum Bengals) * 1000 = Bears
                       | x >= (fromEnum Bengals) * 1000 && x < (fromEnum Browns) * 1000 = Bengals
                       | x >= (fromEnum Browns) * 1000 && x < (fromEnum Cowboys) * 1000 = Browns
                       | x >= (fromEnum Cowboys) * 1000 && x < (fromEnum Broncos) * 1000 = Cowboys
                       | x >= (fromEnum Broncos) * 1000 && x < (fromEnum Lions) * 1000 = Broncos
                       | x >= (fromEnum Lions) * 1000 && x < (fromEnum Packers) * 1000 = Lions
                       | x >= (fromEnum Packers) * 1000 && x < (fromEnum Texans) * 1000 = Packers
                       | x >= (fromEnum Texans) * 1000 && x < (fromEnum Colts) * 1000 = Texans
                       | x >= (fromEnum Colts) * 1000 && x < (fromEnum Jaguars) * 1000 = Colts
                       | x >= (fromEnum Jaguars) * 1000 && x < (fromEnum Chiefs) * 1000 = Jaguars
                       | x >= (fromEnum Chiefs) * 1000 && x < (fromEnum Chargers) * 1000 = Chiefs
                       | x >= (fromEnum Chargers) * 1000 && x < (fromEnum Rams) * 1000 = Chargers
                       | x >= (fromEnum Rams) * 1000 && x < (fromEnum Dolphins) * 1000 = Rams
                       | x >= (fromEnum Dolphins) * 1000 && x < (fromEnum Vikings) * 1000 = Dolphins
                       | x >= (fromEnum Vikings) * 1000 && x < (fromEnum Patriots) * 1000 = Vikings
                       | x >= (fromEnum Patriots) * 1000 && x < (fromEnum Saints) * 1000 = Patriots
                       | x >= (fromEnum Saints) * 1000 && x < (fromEnum Giants) * 1000 = Saints
                       | x >= (fromEnum Giants) * 1000 && x < (fromEnum Jets) * 1000 = Giants
                       | x >= (fromEnum Jets) * 1000 && x < (fromEnum Raiders) * 1000 = Jets
                       | x >= (fromEnum Raiders) * 1000 && x < (fromEnum Eagles) * 1000 = Raiders
                       | x >= (fromEnum Eagles) * 1000 && x < (fromEnum Steelers) * 1000 = Eagles
                       | x >= (fromEnum Steelers) * 1000 && x < (fromEnum Niners) * 1000 = Steelers
                       | x >= (fromEnum Niners) * 1000 && x < (fromEnum Seahawks) * 1000 = Niners
                       | x >= (fromEnum Seahawks) * 1000 && x < (fromEnum Buccaneers) * 1000 = Seahawks
                       | x >= (fromEnum Buccaneers) * 1000 && x < (fromEnum Titans) * 1000 = Buccaneers
                       | x >= (fromEnum Titans) * 1000 && x < (fromEnum Redskins) * 1000 = Titans
                       | x >= (fromEnum Redskins) * 1000 = Redskins
                       | otherwise = General

getFactLength :: FilePath -> IO Int
getFactLength fp = do
    ls <- readInFileToList fp
    let listLength = length ls
    return listLength

----------------------------
--Lookup Section
----------------------------
        
lookupFact :: Int -> [(Int,Text)] -> Maybe Text
lookupFact key assocList = do 
    let fact = lookup key assocList
    let fullFact = lookupFact' key assocList fact
    if isJust fullFact
        then fullFact
            else Nothing

lookupFact' :: Int -> [(Int,Text)] -> Maybe Text -> Maybe Text
lookupFact' key assocList fact = do 
    if isJust fact
        then Just ((Text.pack . show) (determineTeamFromKey key) <> Text.pack " Fact: " <> fromJust fact)
            else Nothing
            
lookupTeamFact :: Int -> [(Int,Text)] -> Maybe Text
lookupTeamFact key assocList = lookup key assocList

--ReadIn File Section

fileLogReader :: FilePath -> IO Text
fileLogReader fp = do 
    --Read File Lazily
    content <- readFile fp
    let txtContent = Text.pack content 
    return txtContent

fileReader :: FilePath -> IO [(Int, Text)]
fileReader fp = do 
    --putStrLn "[fileReader]: fileReader starting using given file path"
    --Read File Lazily
    content <- readFile fp
    --Converts single long input string to separate line list
    lineList <- fmap lines (readFile fp)
    --putStrLn "[fileReader]: Converting lines to Associated List"
    --Convert read in content to assoc list
    let assocList = map splitLineToAssoc lineList
    --putStrLn "[fileReader]: Converted to Associated List"
    return assocList

readInFileToList :: FilePath -> IO [String]
readInFileToList fp = do 
    ls <- fmap lines (readFile fp)
    return ls

splitLineToAssoc :: String -> (Int, Text)
splitLineToAssoc input = toAssoc (splitOn "/s/" input)
    where 
        toAssoc :: [String] -> (Int, Text)
        toAssoc [] = (-1,"Invalid Fact")
        toAssoc [x] = (read x :: Int, "Missing Fact")
        toAssoc [x,y] = (read x :: Int, Text.pack y) 