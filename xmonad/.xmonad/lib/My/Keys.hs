{-# LANGUAGE NoMonomorphismRestriction #-}

module My.Keys (myKeys, focusFollow) where

import Control.Monad
import Control.Monad.Writer
import Data.List
import qualified Data.Map as M

import XMonad
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

import My.Topics (myTopicConfig, spawnShell)

-- Keys --

myKeys c =
    [("M-<Left>"   , withFocused $ snapMove L Nothing  )
    ,("M-<Right>"  , withFocused $ snapMove R Nothing  )
    ,("M-<Up>"     , withFocused $ snapMove U Nothing  )
    ,("M-<Down>"   , withFocused $ snapMove D Nothing  )
    ,("M-S-<Left>" , withFocused $ snapShrink R Nothing)
    ,("M-S-<Right>", withFocused $ snapGrow   R Nothing)
    ,("M-S-<Up>"   , withFocused $ snapShrink D Nothing)
    ,("M-S-<Down>" , withFocused $ snapGrow   D Nothing)

    , ("M-l", withFocused (sendMessage . expandWindowAlt) >> sendMessage Expand)
    , ("M-h", withFocused (sendMessage . shrinkWindowAlt) >> sendMessage Shrink)

    ,("M-;", withFocused (sendMessage . tallWindowAlt) >> sendMessage Taller)
    ,("M-o", withFocused (sendMessage . wideWindowAlt) >> sendMessage Wider )

    ,("M-v", toggleFF)

    ,("M-S-b", restart "/home/aavogt/bin/obtoxmd" True)
    ,("M-S-d", restart "urxvt -e xmonad" False)

    ,("M-S-o"  , withFocused $ sendMessage . UnMerge   )
    ,("M-S-C-o", withFocused $ sendMessage . UnMergeAll)
    ,("M-C-m"  , withFocused $ sendMessage . MergeAll  )
    ,("M-C-."  , onGroup W.focusDown')
    ,("M-C-,"  , onGroup W.focusUp'  )

    ,("M-p",  shellPromptHere myXPConfig)
    ,("M-x", submap $ M.fromList subMaps)
    ,("M-g", submap $ defaultSublMap c  )

    ,("M-S-.", focusDown)
    ,("M-S-,", focusUp  )

    ,("M-S-a", currentTopicAction myTopicConfig)
    ,("M-a", warpToCentre >> goToSelected gsConfig)
    -- workaround
    ,("M-<Tab>", switchNthLastFocused myTopicConfig . succ . length . W.visible . windowset =<< get )

    ,("M-s"  , warpToCentre >> promptedGoto )
    ,("M-S-s", warpToCentre >> promptedShift)

    ,("M-b", sendMessage ToggleStruts)
    ,("M-<Return>", dwmpromote)
    ,("M-S-<Return>", spawnShell)
    -- don't force a recompile, if nothing has changed (xmonad --recompile runs XMonad.recompile True)
    ,("M-q", spawn "ghc -e ':m +XMonad Control.Monad System.Exit' -e 'flip unless exitFailure =<< recompile False' && xmonad --restart")
    ,("M-S-q", spawn "~/wip/x11-wm/xmonad/rebuild.sh")
    ,("<Print>",  spawn "scrot")
    ]
    ++
    concatMap (\(m,f) -> lrud ("M-"++m) f)
        [("S-"  , sendMessage . Swap)
        ,("C-"  , sendMessage . pullGroup)
        ,("S-C-", sendMessage . pushWindow)
        ,(""    , sendMessage . Go)]
    ++ mediaKeys ++
    [("M-"++m++[key], screenWorkspace sc >>= flip whenJust (windows . f))
        | (f, m) <- [(W.view, ""), (W.shift, "S-")]
        , (key, sc) <- zip "wf" [0 .. ]]
    ++
    [ ("M-"++m++[k], a i)
        | (a, m) <- [(switchNthLastFocused myTopicConfig,""),(shiftNthLastFocused, "S-")]
        , (i, k) <- zip [1..] "123456789"]

-- helper for windowNavigation keys
--    note: with colemak neiu are placed where jkli are with qwerty layout
lrud :: String -> (Direction2D -> b) -> [(String, b)]
lrud m cmd = zip ks cmds
    where
      ks   = map (\x -> m ++ [x]) "niue"
      cmds = map cmd [L,R,U,D]
      -- cmds = zipWith ($) (repeat cmd) [L,R,U,D]

subMaps = [((0, xK_o),  runOrRaisePrompt myXPConfig),
           ((0, xK_p),  shellPromptHere myXPConfig),
           ((0, xK_x), xmonadPrompt myXPConfig),
           ((0, xK_z), sshPrompt myXPConfig),
           ((shiftMask, xK_w), windowPromptGoto myXPConfig),
           ((0, xK_w), promptSearch myXPConfig wikipedia),
           ((0, xK_s), promptSearch myXPConfig multi),
           ((0, xK_m), promptSearch myXPConfig mathworld),
           ((0, xK_b), sendMessage ToggleStruts),
           ((0, xK_f), withFocused $ windows . W.sink),
           ((0, xK_v), refresh),
           ((0, xK_c), asks config >>= spawnHere . terminal),
           ((0, xK_k), kill)
           ]


amarok = False

mediaKeys = [("<XF86AudioPlay>", do mpcAct "toggle"; when amarok $ spawn "amarok -t"),
             ("<XF86AudioStop>", promptHost),
             ("<XF86AudioNext>", do mpcAct "next"; when amarok $ spawn "amarok -f"),
             ("<XF86AudioPrev>", do mpcAct "prev"; when amarok $ spawn "amarok -r"),
             ("<XF86AudioMute>", spawn "ossmix vmix0-outvol 0"),
             ("<XF86AudioLowerVolume>",   spawn "amixer sset PCM 1-"),
             ("<XF86AudioRaiseVolume>",   spawn "amixer sset PCM 1+"),
             ("<XF86Sleep>", spawn "sudo pm-suspend"),
             ("<XF86MonBrightnessUp>", spawn "brightness up"),
             ("<XF86MonBrightnessDown>", spawn "brightness down")
             ]
    where mpcAct c = do
            h <- XS.gets hostPrompt
            spawn $ unwords ["export MPD_HOST="++h,";","mpc",c]

-- Prompt for mpd host
newtype HostPrompt = HostPrompt { hostPrompt :: String } deriving (Read,Show,Typeable)
instance ExtensionClass HostPrompt where
    initialValue = HostPrompt "/home/aavogt/.mpd/socket"
    extensionType = PersistentExtension

instance XPrompt HostPrompt where showXPrompt _ = "Pick MPD Host: "
promptHost = mkXPrompt (HostPrompt "") myXPConfig (return . compl) (XS.put . HostPrompt)
    where compl s = nub $ filter (s `isPrefixOf`) ["127.0.0.1","dell"]

toggleFF = XS.modify $ FocusFollow . not . getFocusFollow

warpToCentre = gets (W.screen . W.current . windowset) >>= \x -> warpToScreen x  0.5 0.5

wsgrid = gridselect gsConfig <=< asks $ map (\x -> (x,x)) . workspaces . config

promptedGoto = wsgrid >>= flip whenJust (switchTopic myTopicConfig)

promptedShift = wsgrid >>= \x -> whenJust x $ \y -> windows (W.greedyView y . W.shift y)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- A nice little example of extensiblestate
newtype FocusFollow = FocusFollow {getFocusFollow :: Bool } deriving (Typeable,Read,Show)
instance ExtensionClass FocusFollow where
    initialValue = FocusFollow True
    extensionType = PersistentExtension

-- this eventHook is the same as from xmonad for handling crossing events
focusFollow e@(CrossingEvent {ev_window=w, ev_event_type=t})
                | t == enterNotify, ev_mode e == notifyNormal =
        whenX (XS.gets getFocusFollow) (focus w) >> return (All True)
focusFollow _ = return (All True)


myXPConfig :: XPConfig
myXPConfig = def { font = "xft:Profont:pixelsize=15:autohint=true" }

gsConfig = def { gs_navigate = fix $ \self ->
    let navKeyMap = M.mapKeys ((,) 0) $ M.fromList $
                [(xK_Escape, cancel)
                ,(xK_Return, select)
                ,(xK_slash , substringSearch self)]
           ++
            map (\(k,a) -> (k,a >> self))
                [(xK_Left  , move (-1,0 ))
                ,(xK_h     , move (-1,0 ))
                ,(xK_n     , move (-1,0 ))
                ,(xK_Right , move (1,0  ))
                ,(xK_l     , move (1,0  ))
                ,(xK_i     , move (1,0  ))
                ,(xK_Down  , move (0,1  ))
                ,(xK_j     , move (0,1  ))
                ,(xK_e     , move (0,1  ))
                ,(xK_Up    , move (0,-1 ))
                ,(xK_u     , move (0,-1 ))
                ,(xK_y     , move (-1,-1))
                ,(xK_m     , move (1,-1 ))
                ,(xK_space , setPos (0,0))
                ]
    in makeXEventhandler $ shadowWithKeymap navKeyMap (const self) }
