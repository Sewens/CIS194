{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

{-
Parse String into LogMessage
1. unwords line get first three word
2. check first words to determine message type
3. gen LogMessage by the rest
-}

parseMessageType :: String -> MessageType
parseMessageType tStr 
            |
            whre x:xxs = words xs
parseMessage :: String -> LogMessage
parseMessage xs
            | head xss `elem` ["I", "W", "E"] = LogMessage (parseMessageType ) (read $ xss!!2) (unwords $ drop 3 xss)
            | otherwise = 1
            where xss = words xs