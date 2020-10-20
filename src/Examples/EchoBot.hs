module Examples.EchoBot where

import           ChatEvents
import           ExtIrcBot
import           Utils

run :: IO ()
run = runBot BotSettings
  { host ="irc.freenode.net"
  , port = 6667
  , name = "MyEchoBot"
  , channels = ["#botwar"]
  , handler =  handleEvent
  , async_handlers = False
  }

handleEvent :: Handler
handleEvent (ChannelMessage u c m) = return $ SendChannelMessage c (humanizeName u ++ " said: " ++ m)
handleEvent _ = return NoAction
