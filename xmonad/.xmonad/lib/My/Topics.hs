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
  , "irc"
  , "xm-conf"
  , "gimp"
  , "gitit"
  , "admin"
  , "movie"
  , "music"
  , "pdf"
  , "doc"
  , "xmc"
  , "xme"
  , "xm"
  , "xmobar"
  , "wip"
  , "test"
  ]


myTopicConfig = TopicConfig
  { topicDirs = M.fromList $
      [ ("a", "./")
      , ("haskell", "haskell")
      , ("xm-conf", ".xmonad")
      , ("xme", "wip/x11-wm/xmonad/extras/xmonad-extras/XMonad")
      , ("xm", "wip/x11-wm/xmonad/core/xmonad")
      , ("xmc", "wip/x11-wm/xmonad/contrib/XMonadContrib/XMonad")
      , ("xmobar", "wip/x11-wm/xmobar")
      , ("movie", "media/movie")
      , ("music", "media/music")
      , ("doc", "doc")
      , ("pdf", "ref")
      , ("gitit", "wip/gitit")
      , ("gimp", "./")
      , ("wip", "wip")
      ]
  , defaultTopicAction = const $ spawnShell >*> 2
  , defaultTopic = "a"
  , maxTopicHistory = 10
  , topicActions = M.fromList $
      [ ("xm-conf", spawnShellIn ".xmonad/lib/XMonad/Layout" >>
                        spawn "urxvt -e vim ~/.xmonad/xmonad.hs")
       , ("xmc"    , spawnShell >*> 2)
       , ("xmobar" , spawnShellIn "wip/x11-wm/xmobar/Plugins" >*> 2)
       , ("music"  , spawn "urxvt -e ncmpc -h /home/aavogt/.mpd/socket" >> spawn "export MPD_HOST=192.168.1.2; mpc && urxvt -e ncmpc -h 192.168.1.2")
       , ("mail"   , spawnOn "mail" "urxvt -e mutt")
       , ("irc"    , spawnOn "irc" "urxvt --title irc -e ssh engage")
       , ("web"    , spawnOn "web" "firefox")
       , ("pdf"    , spawnOn "pdf" "okular")
       , ("gimp"   , spawnHere "gimp")
      ]
  }

-- From the sample config in TopicSpace, these should probably be exported from that module
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn dir = do
    -- color <- randomBg' (HSV 255 255)
    t <- asks (terminal . config)
    spawnHere $ "cd " ++ dir ++ " && " ++ t -- ++ " -bg " ++ color
