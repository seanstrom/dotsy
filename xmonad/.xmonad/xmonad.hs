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

-- import XMonad.Layout.Drawer
-- import XMonad.Layout.Grid
-- import XMonad.Layout.IM
-- import XMonad.Layout.Magnifier
-- import XMonad.Layout.Master
-- import XMonad.Layout.MouseResizableTile
-- import XMonad.Layout.Named
-- import XMonad.Layout.NoBorders
-- import XMonad.Layout.PerWorkspace
-- import XMonad.Layout.Simplest
-- import XMonad.Layout.SimplestFloat
-- import XMonad.Layout.Tabbed
-- import XMonad.Layout.TrackFloating

import Control.Applicative
import Control.Monad
import Control.Monad.Instances ()
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Data.Traversable(traverse)
import Graphics.X11.Xinerama
import Graphics.X11.Xlib
import System.IO

import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Actions.SpawnOn
import XMonad.Actions.TopicSpace
-- import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.SubLayouts
import XMonad.Util.EZConfig
import XMonad.Util.Replace
import XMonad.Util.Run
import qualified XMonad.StackSet as W

import My.Layout (myLayout)
import My.Topics (myTopics, myTopicConfig, spawnShell, spawnShellIn)
import My.Keys (myKeys, focusFollow)
import My.DynamicBars

main :: IO ()
main = do
  replace
  checkTopicConfig myTopics myTopicConfig
  xmonad . ewmh . myUrgencyHook $ myConfig

statusBarStart :: DynamicStatusBar
statusBarStart (S id) = spawnPipe $ "/usr/bin/env xmobar -x " ++ show id

statusBarClean :: DynamicStatusBarCleanup
statusBarClean = safeSpawn "pkill -f" ["xmobar"]

myTerminal = "termite"

myConfig = additionalKeysP configObj (myKeys configObj)
  where configObj = def {
                    focusFollowsMouse  = True
                  , handleEventHook    = myEventHook
                  , layoutHook         = myLayout
                  , logHook            = myLogHook
                  , manageHook         = myManageHook
                  , modMask            = mod4Mask
                  , startupHook        = myStartupHook configObj
                  , terminal           = myTerminal
                  , workspaces         = myTopics
                  , borderWidth = 4
                  , focusedBorderColor = "#6A555C" --"#404752"
                  , normalBorderColor = "#404752" --"#343C48"
                  }
-- Hooks --

myUrgencyHook :: (LayoutClass l Window) => XConfig l -> XConfig l
myUrgencyHook = withUrgencyHook FocusHook
-- urgencyHook = withUrgencyHook NoUrgencyHook

myEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook <+> focusFollow <+> docksEventHook <+> ppEventHook <+> customEventHook
  where customEventHook e@PropertyEvent { ev_window = w } = do
          isURXVT <- runQuery (className =? "URxvt") w
          if not isURXVT then hintsEventHook e else return (All True)
        customEventHook _ = return (All True)
        ppEventHook = dynStatusBarEventHook statusBarStart statusBarClean

myLogHook = do
  multiPPFormat
    (mergePPOutputs [ XMonad.Actions.TopicSpace.pprWindowSet myTopicConfig
                    , dynamicLogString . onlyTitle
                    ])
    myPP
    myPP{ ppTitle = const "" }
  updatePointer (0.5, 0.5) (0.2, 0.2)

-- myLogHook handles = do
--   multiPP'
--     (mergePPOutputs [ XMonad.Actions.TopicSpace.pprWindowSet myTopicConfig
--                     , dynamicLogString . onlyTitle
--                     ])
--     myPP
--     myPP{ ppTitle = const "" }
--     handles
--   updatePointer (0.5, 0.5) (0.2, 0.2)

myManageHook = mconcat [ manageSpawn
                       , isFullscreen --> doFullFloat
                       , className =? "Xterm" --> queryMerge (className =? "Xterm")
                       , manageDocks
                       ]

myStartupHook cfg = do
  dynStatusBarStartup statusBarStart statusBarClean
  checkKeymap myConfig (myKeys cfg)
  return ()

-- Not Understood / As Is

data ExpandEdges a = ExpandEdges Int deriving (Read,Show)

instance LayoutModifier ExpandEdges Window where
    modifyLayout (ExpandEdges n) ws (Rectangle x y w h) = let
            bigRect = Rectangle (x - fromIntegral n) (y - fromIntegral n)
                                (w + 2*fromIntegral n) (h + 2*fromIntegral n)
        in
        runLayout ws bigRect

-- | push edges off-screen
expandEdges n layout = ModifiedLayout (ExpandEdges n) layout

-------------------- Support for per-screen xmobars ---------
-- Some parts of this should be merged into contrib sometime

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
--------------------------------------------------------------

{- | Sometimes this picks the wrong element to merge into (that is, not the
'focused' element of the group), and SubLayouts breaks up the whole group
-}
queryMerge pGrp = do
    w <- ask
    aws <- liftX $ filterM (runQuery pGrp) =<< gets
        (W.integrate' . W.stack . W.workspace . W.current . windowset)

    let addRem = False -- run the query with window removed??
    when addRem
        (liftX $ modify (\ws -> ws { windowset = W.insertUp w (windowset ws) }))
    liftX $ windows (W.insertUp w)

    mapM_ (liftX . sendMessage . XMonad.Layout.SubLayouts.Merge w) aws

    when addRem
        (liftX $ modify (\ws -> ws { windowset = W.delete' w (windowset ws) }))

    idHook

