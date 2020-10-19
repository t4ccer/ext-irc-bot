{-# LANGUAGE DuplicateRecordFields #-}

module ChatEvents where


data ChatEvent =
    UserJoined
    { get_user    :: String
    , get_channel :: String
    }
  | UserLeft
    { get_user    :: String
    , get_channel :: String
    }
  | ChannelMessage
    { get_author  :: String
    , get_channel :: String
    , get_message :: String
    }
  | PrivMessage
  { get_author  :: String
  , get_message :: String
  }
  | OtherEvent String
  deriving Show

data ChatAction =
    NoAction
  | SendChannelMessage
    { get_channel :: String
    , get_message :: String
    }
  | SendPrivMessage
    { get_user    :: String
    , get_message :: String
    }
  deriving Show

