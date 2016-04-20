module My.Topics (myTopics, myTopicConfig, spawnShell, spawnShellIn) where

import qualified Data.Map as M

import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Actions.TopicSpace

myTopics :: [Topic]
myTopics =
  [ "a"
  , "web"
  , "haskell"
  , "mail"
  , "chat"
  , "writing"
  , "movie"
  , "music"
  ]


myTopicConfig = TopicConfig
  { topicDirs = M.fromList $
      [ ("a", "./")
      , ("haskell", "dev/lyah")
      , ("writing", "dev/wiki")
      ]
  , defaultTopicAction = const $ spawnShell >*> 1
  , defaultTopic = "a"
  , maxTopicHistory = 10
  , topicActions = M.fromList $
      [ ("web" , spawnOn "web"  "chromium")
      , ("chat", spawnOn "chat" "slack")
      ]
  }

-- From the sample config in TopicSpace, these should probably be exported from that module
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn dir = do
    -- color <- randomBg' (HSV 255 255)
    t <- asks (terminal . config)
    spawnHere $ "cd " ++ dir ++ " && " ++ t -- ++ " -bg " ++ color
