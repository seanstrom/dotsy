{-# LANGUAGE
     FlexibleContexts,
     FlexibleInstances,
     MultiParamTypeClasses,
     NoMonomorphismRestriction,
     ScopedTypeVariables,
     TypeSynonymInstances,
     UndecidableInstances
     #-}
{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Instances ()
import Control.Monad.Writer
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Traversable(traverse)
import Graphics.X11.Xinerama
import Graphics.X11.Xlib
import System.IO
import System.Taffybar.Hooks.PagerHints (pagerHints)

import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Actions.SpawnOn
import XMonad.Actions.TopicSpace
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BoringWindows
import XMonad.Layout.Drawer
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
-- import XMonad.Layout.IM
import XMonad.Layout.Magnifier
import XMonad.Layout.Mosaic
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.TrackFloating
import XMonad.Util.EZConfig
import XMonad.Util.Replace
import XMonad.Util.Run
import qualified XMonad.StackSet as W

import XMonad.Actions.DwmPromote
import XMonad.Actions.FloatSnap
import XMonad.Actions.GridSelect
import XMonad.Actions.Search
import XMonad.Actions.SpawnOn
import XMonad.Actions.Submap
import XMonad.Actions.TopicSpace
import XMonad.Actions.Warp
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BoringWindows
import XMonad.Layout.Mosaic
import XMonad.Layout.MosaicAlt
import XMonad.Layout.SubLayouts
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

-- import My.Layout (myLayout)
-- import My.Topics (myTopics, myTopicConfig, spawnShell, spawnShellIn)
import My.Keys (myKeys, focusFollow)
import My.DynamicBars

main :: IO ()
main = do
  replace
  checkTopicConfig myTopics myTopicConfig
  xmonad . ewmh . pagerHints . myUrgencyHook $ myConfig

myConfig = additionalKeysP configObj (myKeys configObj)
  where configObj = def {
                    focusFollowsMouse  = True
                  , borderWidth = 4
                  , focusedBorderColor = "#6A555C" --"#404752"
                  , normalBorderColor = "#404752" --"#343C48"
                  , handleEventHook    = myEventHook
                  , layoutHook         = myLayout
                  -- , logHook            = myLogHook
                  , manageHook         = myManageHook
                  , modMask            = mod4Mask
                  , startupHook        = myStartupHook configObj
                  , terminal           = myTerminal
                  , workspaces         = myTopics
                  }

myTerminal = "termite"

myStartupHook cfg = do
  dynStatusBarStartup statusBarStart statusBarClean
  checkKeymap myConfig (myKeys cfg)
  return ()

statusBarStart :: DynamicStatusBar
statusBarStart (S id) = spawnPipe $ "TAFFY_SCREEN=" ++ show id ++ " /usr/bin/env taffybar"

statusBarClean :: DynamicStatusBarCleanup
statusBarClean = do
  unsafeSpawn "killall taffybar-linux-x86_64"
  return ()

-- Layout

myLayout =
  trackFloating
  . avoidStruts
  . smartSpacing 3
  . smartBorders
  . onWorkspace "movie" (magnifier m ||| layoutHints Full)
  $ m ||| named "F" (noBorders Full) ||| named "G" Grid
  where
    m = named "M"
      . lessBorders Screen
      . layoutHintsToCenter
      . addTabs shrinkText defaultTheme
      . boringAuto
      . subLayout [] (Simplest ||| simplestFloat)
      $ mosaic 1.5 [7,5,2]

-- Topics

myTopics =
  [ "code"
  , "web"
  , "fp"
  , "mail"
  , "chat"
  , "writing"
  , "movie"
  , "music"
  ]


myTopicConfig = TopicConfig
  { topicDirs = M.fromList []
  , defaultTopicAction = const $ spawnShell >*> 1
  , defaultTopic = "code"
  , maxTopicHistory = 10
  , topicActions = M.fromList $
      [ ("web" , spawnOn "web"  "chromium")
      , ("chat", spawnOn "chat" "slack")
      ]
  }

-- Hooks

myUrgencyHook :: (LayoutClass l Window) => XConfig l -> XConfig l
myUrgencyHook = withUrgencyHook FocusHook
-- urgencyHook = withUrgencyHook NoUrgencyHook

myEventHook :: Event -> X All
myEventHook = foldl1 (<+>) eventHooks
  where ppEventHook = dynStatusBarEventHook statusBarStart statusBarClean
        eventHooks = [ ewmhDesktopsEventHook
                     , fullscreenEventHook
                     , focusFollow
                     , docksEventHook
                     -- , ppEventHook
                     , hintsEventHook
                     ]

myManageHook = composeAll manageHooks
  where manageHooks = [ manageHook defaultConfig
                      , manageDocks
                      , manageSpawn
                      , isFullscreen --> doFullFloat
                      ]

myLogHook = do
  multiPPFormat
    (mergePPOutputs [ XMonad.Actions.TopicSpace.pprWindowSet myTopicConfig
                    , dynamicLogString . onlyTitle
                    ])
    myPP
    myPP{ ppTitle = const "" }
  updatePointer (0.5, 0.5) (0.2, 0.2)

mergePPOutputs :: [PP -> X String] -> PP -> X String
mergePPOutputs x pp = fmap (intercalate (ppSep pp)) . sequence . sequence x $ pp

onlyTitle :: PP -> PP
onlyTitle pp = defaultPP { ppCurrent = const ""
                         , ppHidden = const ""
                         , ppVisible = const ""
                         , ppLayout = ppLayout pp
                         , ppTitle = ppTitle pp }

myPP :: PP
myPP = sjanssenPP { ppLayout = xmobarColor "orange" "", ppUrgent = xmobarColor "red" "" . ('^':) }

-- From the sample config in TopicSpace, these should probably be exported from that module
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn dir = do
    t <- asks (terminal . config)
    spawnHere $ "cd " ++ dir ++ " && " ++ t -- ++ " -bg " ++ color

