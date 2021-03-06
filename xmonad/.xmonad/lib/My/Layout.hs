{-# LANGUAGE
     FlexibleContexts,
     FlexibleInstances,
     MultiParamTypeClasses,
     NoMonomorphismRestriction,
     ScopedTypeVariables,
     TypeSynonymInstances,
     UndecidableInstances
     #-}

module My.Layout (myLayout) where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BoringWindows
import XMonad.Layout.Drawer
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Magnifier
import XMonad.Layout.Master
import XMonad.Layout.Mosaic
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.TrackFloating
import XMonad.Layout.WindowNavigation
-- import XMonad.Util.WindowProperties

myLayout =
  trackFloating
  . avoidStruts
  . smartSpacing 3
  . smartBorders
  . onWorkspace "movie" (magnifier m ||| layoutHints Full)
  $ m ||| named "F" (noBorders Full)
      where nav = configurableNavigation (navigateColor "#ffff00")
            m = named "M"
              . lessBorders Screen
              . layoutHintsToCenter
              . addTabs shrinkText defaultTheme
              . boringAuto
              . subLayout [] (Simplest ||| simplestFloat)
              $ mosaic 1.5 [7,5,2]
            gimp = nav
                 . onLeft (simpleDrawer 0.01 0.3 $ Role "gimp-toolbox")
                 . withIM 0.15 (Role "gimp-dock")
                 . addTabs shrinkText defaultTheme
                 . nav
                 . boringAuto
                 . subLayout [] Simplest
                 $ mouseResizableTile ||| Full


data ExpandEdges a = ExpandEdges Int deriving (Read,Show)

instance LayoutModifier ExpandEdges Window where
    modifyLayout (ExpandEdges n) ws (Rectangle x y w h) = let
            bigRect = Rectangle (x - fromIntegral n) (y - fromIntegral n)
                                (w + 2*fromIntegral n) (h + 2*fromIntegral n)
        in
        runLayout ws bigRect

-- | push edges off-screen
expandEdges n layout = ModifiedLayout (ExpandEdges n) layout
