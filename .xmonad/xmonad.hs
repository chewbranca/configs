-- ~/.xmonad/xmonad.hs
-- Imports {{{
import XMonad
-- Prompt
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)
-- Hooks
import XMonad.Operations

import System.IO
import System.Exit

import XMonad.Util.Run


import XMonad.Actions.CycleWS

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
-- import XMonad.Layout.Gaps
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Reflect
import XMonad.Layout.Grid

import Data.Ratio ((%))

import qualified XMonad.StackSet as W
import qualified Data.Map as M

--}}}

-- Config {{{
-- Define Terminal
myTerminal      = "urxvt"
-- Define modMask
modMask' :: KeyMask
modMask' = mod1Mask
-- Define workspaces
myWorkspaces    = ["1:main","2:web","3:movie","4:music","5:gimp","6:chat"]
-- Dzen config
myStatusBar = "dzen2 -x '0' -y '0' -h '24' -w '1920' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"
myBtmStatusBar = "conky -c /home/johan/.conky_bottom_dzen | dzen2 -x '0' -w '1920' -h '24' -ta 'c' -bg '#1B1D1E' -fg '#FFFFFF' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*' -y '1176'"
myBitmapsDir = "/home/johan/.xmonad/dzen"
--}}}
-- Main {{{
main = do
    dzenTopBar <- spawnPipe myStatusBar
    dzenBtmBar <- spawnPipe myBtmStatusBar
    spawn "sh /home/johan/.xmonad/autostart.sh"
    xmonad $ defaultConfig
      { terminal            = myTerminal
      , workspaces          = myWorkspaces
      , keys                = keys'
      , modMask             = modMask'
      , startupHook         = ewmhDesktopsStartup >> setWMName "LG3D"
      , layoutHook          = layoutHook'
      , manageHook          = manageHook'
      , logHook             = myLogHook dzenTopBar >> fadeInactiveLogHook 0xdddddddd  >> setWMName "LG3D"
      , normalBorderColor   = colorNormalBorder
      , focusedBorderColor  = colorFocusedBorder
      , borderWidth   = 3
}
--}}}


-- Hooks {{{
-- ManageHook {{{
manageHook' :: ManageHook
manageHook' = (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    , [className    =? c            --> doShift  "2:web"    |   c   <- myWebs   ] -- move webs to main
    , [className    =? c            --> doShift  "1:main"   |   c   <- myDev   ] -- move dev to main
    , [className    =? c            --> doShift  "3:movie"  |   c   <- myMovie   ] -- move movie to movie
    , [className    =? c            --> doShift  "4:music"  |   c   <- myMusic   ] -- move music to music
    , [className    =? c            --> doShift  "5:gimp"    |   c   <- myWork   ] -- move img to div
    , [className    =? c            --> doShift	 "6:chat"   |   c   <- myChat  ] -- move chat to chat
--    , [name         =? "ncmpcpp"      --> doShift  "4:music"]
    , [className    =? c            --> doCenterFloat       |   c   <- myFloats ] -- float my floats
    , [name         =? n            --> doCenterFloat       |   n   <- myNames  ] -- float my names
    , [isFullscreen                 --> myDoFullFloat                           ]
    ]) 

    where

        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"

        -- classnames
        myFloats  = ["Smplayer","MPlayer","VirtualBox","Xmessage","XFontSel","Downloads","Nm-connection-editor"]
        myWebs    = ["Navigator","Shiretoko","Firefox","Uzbl","uzbl","Uzbl-core","uzbl-core","Google-chrome","Chromium","Shredder","Mail"]
        myMovie   = ["Boxee","Trine"]
	myMusic	  = ["Rhythmbox","Tomahawk","Banshee","Banshee Media Player","banshee-1","Exaile","Spotify"]
	myChat	  = ["Pidgin","Buddy List"]
	myWork	  = ["Gimp"]
	myDev	  = ["urxvt"]

        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]

        -- names
        myNames   = ["bashrun","Google Chrome Options","Chromium Options"]

-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat
-- }}}
-- layoutHook' = customLayout
layoutHook'  =  onWorkspaces ["1:main","4:music"] customLayout $ onWorkspaces ["5:gimp"] gimpLayout $ onWorkspaces ["6:chat"] imLayout $
                customLayout2
-- Bar
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#ebac54" "#1B1D1E" . pad
      , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
      , ppHidden            =   dzenColor "white" "#1B1D1E" . pad
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
      , ppUrgent            =   dzenColor "red" "#1B1D1E" . pad
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppLayout            =   dzenColor "#ebac54" "#1B1D1E" .
                                (\x -> case x of
                                    "ResizableTall"             ->      "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                                    "Mirror ResizableTall"      ->      "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                                    "Full"                      ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
                                    "Simple Float"              ->      "~"
                                    _                           ->      x
                                )
      , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }
-- Layout
customLayout = avoidStruts $ smartBorders tiled ||| smartBorders (Mirror tiled) ||| noBorders Full ||| smartBorders simpleFloat
  where
    --tiled = ResizableTall 1 (2/100) (1/2) []
    tiled   = spacing pxl $ ResizableTall 1 (2/100) (1/2) []
--    nmaster = 1   
--    delta   = 2/100
--    ratio   = 1/2
    pxl     = 5

customLayout2 = avoidStruts $ noBorders Full ||| smartBorders tiled ||| smartBorders (Mirror tiled) ||| smartBorders simpleFloat
  where
    --tiled = ResizableTall 1 (2/100) (1/2) []
    tiled   = spacing pxl $ ResizableTall 1 (2/100) (1/2) []
--    nmaster = 1   
--    delta   = 2/100
--    ratio   = 1/2
    pxl     = 5

gimpLayout  = avoidStruts $ withIM (0.11) (Role "gimp-toolbox") $
              reflectHoriz $
              withIM (0.15) (Role "gimp-dock") Full

imLayout    = avoidStruts $ withIM (1%5) (And (ClassName "Pidgin") (Role "buddy_list")) Grid 
--  where
--    tiled   = spacing pxl $ ResizableTall 1 (2/100) (1/2) []
--    nmaster = 1
--    delta   = 2/100
--    ratio   = 1/2
--    pxl     = 5
--}}}
-- Theme {{{
-- Color names are easier to remember:
--colorOrange          = "#ff7701"
colorOrange         = "#FD971F"
--colorDarkGray        = "#171717"
colorDarkGray       = "#1B1D1E"
--colorPink            = "#e3008d"
colorPink           = "#F92672"
--colorGreen           = "#00aa4a"
colorGreen          = "#A6E22E"
--colorBlue            = "#008dd5"
colorBlue           = "#66D9EF"
--colorYellow          = "#fee100"
colorYellow         = "#E6DB74"
--colorWhite           = "#cfbfad"
colorWhite          = "#CCCCC6"
 
--colorNormalBorder    = "#1c2636"
colorNormalBorder   = "#CCCCC6"
--colorFocusedBorder   = "#ebac54"
colorFocusedBorder  = "#fd971f"


barFont  = "terminus"
barXFont = "inconsolata:size=14"
xftFont = "xft: inconsolata-14"
--}}}

-- Prompt Config {{{
mXPConfig :: XPConfig
mXPConfig =
    defaultXPConfig { font                  = barFont
                    , bgColor               = colorDarkGray
                    , fgColor               = colorGreen
                    , bgHLight              = colorGreen
                    , fgHLight              = colorDarkGray
                    , promptBorderWidth     = 0
                    , height                = 14
                    , historyFilter         = deleteConsecutive
                    }
 
-- Run or Raise Menu
largeXPConfig :: XPConfig
largeXPConfig = mXPConfig
                { font = xftFont
                , height = 22
                }
-- }}}
-- Key mapping {{{
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,                    xK_p        ), runOrRaisePrompt largeXPConfig)
    , ((mod1Mask,                   xK_F2       ), spawn "gmrun")
    -- Programs
    , ((0,                          xK_Print    ), spawn "scrot -e 'mv $f ~/bilder/screenshots/'")
    , ((modMask,                    xK_r        ), spawn $ XMonad.terminal conf) -- spawn terminal
    , ((modMask,		            xK_w        ), spawn "chromium")
    , ((modMask .|. shiftMask,      xK_p        ), spawn "gimp")
    , ((modMask .|. shiftMask,	    xK_n        ), spawn "spotify")
--    , ((modMask .|. shiftMask,	    xK_m        ), spawn "exaile")
    , ((modMask .|. shiftMask,      xK_m        ), spawn "urxvt -T ncmpcpp -e 'ncmpcpp'")
    , ((modMask .|. shiftMask,      xK_i        ), spawn "gthumb")
    , ((modMask,                    xK_e        ), spawn "nautilus --no-desktop --browser")
    , ((modMask .|. shiftMask,	    xK_b	), spawn "/opt/boxee/Boxee")
    -- Media Keys
    , ((0,                          0x1008ff12  ), spawn "amixer -q sset Master toggle") -- XF86AudioMute
    , ((0,                          0x1008ff11  ), spawn "amixer -q sset Master 655- unmute") -- XF86AudioLowerVolume
    , ((0,                          0x1008ff13  ), spawn "amixer -q sset Master 655+ unmute") -- XF86AudioRaiseVolume
    , ((0,                          0x1008ff14  ), spawn "ncmpcpp toggle")
    , ((0,                          0x1008ff17  ), spawn "ncmpcpp next")
    , ((0,                          0x1008ff16  ), spawn "ncmpcpp prev")

    -- layouts
    , ((modMask,                    xK_space    ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,      xK_space    ), setLayout $ XMonad.layoutHook conf) -- reset layout on current desktop to default
    , ((modMask,                    xK_b        ), sendMessage ToggleStruts)
    , ((mod1Mask,                   xK_Tab      ), windows W.focusDown) -- move focus to next window
    , ((modMask,                    xK_q        ), kill) -- kill selected window
    , ((modMask .|. shiftMask,      xK_j        ), windows W.swapDown) -- swap the focused window with the next window
    , ((modMask .|. shiftMask,      xK_k        ), windows W.swapUp)  -- swap the focused window with the previous window
    , ((modMask .|. shiftMask,      xK_t        ), withFocused $ windows . W.sink) -- Push window back into tiling
    , ((modMask,                    xK_u        ), sendMessage Shrink) -- %! Shrink a master area
    , ((modMask,                    xK_i        ), sendMessage Expand) -- %! Expand a master area
    , ((modMask,                    xK_j        ), sendMessage MirrorShrink) -- %! Shrink a slave area
    , ((modMask,                    xK_k        ), sendMessage MirrorExpand) -- %! Expand a slave area


    -- workspaces
    , ((mod1Mask .|. controlMask,   xK_Right    ), nextWS)
    , ((mod1Mask .|. shiftMask,     xK_Right    ), shiftToNext)
    , ((mod1Mask .|. controlMask,   xK_Left     ), prevWS)
    , ((mod1Mask .|. shiftMask,     xK_Left     ), shiftToPrev)
    
    -- quit, or restart
    , ((modMask .|. shiftMask,      xK_q        ), io (exitWith ExitSuccess))
    , ((modMask .|. shiftMask,      xK_r        ), spawn "killall conky dzen2 && xmonad --recompile && xmonad --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

--}}}
-- vim:foldmethod=marker sw=4 sts=4 ts=4 tw=0 et ai nowrap
