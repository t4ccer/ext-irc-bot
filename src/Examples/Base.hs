module Examples.Base where

import           ChatEvents
import           ExtIrcBot

run :: IO ()
run = runBot BotSettings
  { host ="irc.freenode.net"
  , port = 6667
  , name = "MyBot"
  , channels = []
  , handler =  handleEvent
  , async_handlers = False
  }

handleEvent :: Handler
handleEvent _ = return $ NoAction
