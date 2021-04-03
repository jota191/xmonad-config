import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run          (spawnPipe)
import XMonad.Util.EZConfig     (additionalKeys)
import System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import System.Exit
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.Cross
import XMonad.Layout.Tabbed
import XMonad.Util.Themes
import XMonad.Layout.Spacing
import Kb4000
import Data.List                (isSuffixOf)

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar \
                     \ /home/jpgarcia/.xmonad/xmobarrc -d "
    -- xmproc <- spawnPipe (
    --   "/home/jpgarcia/.cabal/packages/hackage.haskell.org/xmob" ++
    --   "ar/0.33/xmobar-0.33/dist/dist-sandbox-fc83eb03/build/xmobar/xmobar" ++
    --   " /home/jpgarcia/.xmonad/xmobarrc -d")
    xmonad $ def
        { manageHook      = myManageHook
        , layoutHook      = myLayout
        , handleEventHook = handleEventHook def <+> docksEventHook
        , modMask         = mod4Mask
        , keys            = myKeys
        , terminal        = "lxterminal"
        , borderWidth     = 0
        , startupHook     = myStartupHook
        , mouseBindings   = myMouseBindings
        , workspaces      = myWorkspaces
        }

myManageHook = composeAll
    [  className =? "Gimp"  --> doFloat
    , className =? "Gimp" <&&> fmap ("tool" `isSuffixOf`) role --> doFloat
    ]
    <+> manageDocks
    <+> manageHook def
  where role = stringProperty "WM_WINDOW_ROLE"

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- | avoidStruts is used to keep space for xmobarr, Grid and Cross are contrib
myLayout = spacingRaw True (Border 0   bdz bdz bdz)
                      True (Border bdz bdz bdz bdz) True
           $ avoidStruts $   tabbedBottom shrinkText myTheme
                       ||| myTall
                       ||| halfTall
                       ||| Full
                 --    ||| simpleCross  -- contrib:XMonad.Layout.Cross
                 --    ||| simpleTabbed -- contrib:XMonad.Layout.Tabbed
                       ||| Grid         -- contrib:XMonad.Layout.Grid
  where myTall   = Tall 1 (2/100) (13/20) -- <- last one is size of master pane
        halfTall = Tall 1 (2/100) (1/2)
        themes   = [(theme tm) | tm <- listOfThemes]
        bdz      = 0

myTheme = Theme {activeColor = "#2b4f98",
                 inactiveColor = "#cccccc",
                 urgentColor = "#FFFF00",
                 activeBorderColor = "#2b4f98",
                 inactiveBorderColor = "#cccccc",
                 urgentBorderColor = "##00FF00",
                 activeTextColor = "white",
                 inactiveTextColor = "black",
                 urgentTextColor = "#FF0000",
                 fontName = "xft:Noto Sans:pixelsize=10,M+ 1c:pixelsize=10",
                 decoWidth = 200,
                 decoHeight = 16,
                 windowTitleAddons = [],
                 windowTitleIcons = []}

myMouseBindings conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
    -- mod-button1, Set the window to floating mode and move by dragging
      ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)) 
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    -- Move focus to the previous window
    , ((noModMask, 8 :: Button), \_ -> (windows W.focusUp)  )

    -- Move focus to the master window
    , ((noModMask, 9 :: Button), \_ -> (windows W.focusDown))]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- firefox, telegram, emacs, etc
    , ((modm .|. shiftMask, xK_b), spawn  "/opt/icecat/icecat")
    , ((modm .|. shiftMask, xK_t), spawn  "/usr/bin/telegram-desktop")
    , ((modm .|. shiftMask, xK_o), spawn  "emacs")
    , ((modm .|. shiftMask, xK_m), spawn  "thunderbird")
    , ((modm .|. shiftMask, xK_x), spawn  "xchat")
    , ((modm .|. shiftMask, xK_v), spawn  "evince")
    , ((modm .|. shiftMask, xK_f), spawn  "pcmanfm")
    , ((modm .|. shiftMask, xK_i), spawn  "wicd-gtk")
    , ((modm .|. controlMask , xK_Return), spawn  "/opt/bin/cool-retro-term")

    -- Kb 40k bindings
    , ((noModMask, xK_Mail),       spawn  "thunderbird")
    , ((noModMask, xK_WebHome),    spawn  "firefox")
    , ((noModMask, xK_Fav5), spawn  "/home/jpgarcia/.screenlayout/AOC-eDP.sh")
    , ((noModMask, xK_VolUp),      spawn  "amixer -D pulse sset Master 5%+")
    , ((noModMask, xK_VolDown),    spawn  "amixer -D pulse sset Master 5%-")
    , ((noModMask, xK_VolMute),    spawn  "amixer set Master toggle")
    , ((noModMask, xK_Calculator), spawn  "lxterminal --command=ghci")

    -- volume control
    , ((modm .|. shiftMask, xK_equal),     spawn  "amixer -D pulse sset Master 5%+")
    , ((modm .|. shiftMask, xK_minus),    spawn  "amixer -D pulse sset Master 5%-")
    
    
    -- launch dmenu
    , ((modm.|. shiftMask, xK_p),
       spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
 
    -- launch gmrun
    , ((modm, xK_p), spawn "gmrun")

    --take a screenshot of entire display
    , ((modm ,              xK_Print ),
        spawn "scrot ./Pictures/Screenshots/screen_%Y-%m-%d-%H-%M-%S.png -d 1" >>
        spawn "paplay /usr/share/sounds/freedesktop/stereo/screen-capture.oga"
      )

    --take a screenshot of focused window
    , ((modm .|. controlMask, xK_Print ),
       spawn "scrot ./Pictures/Screenshots/window_%Y-%m-%d-%H-%M-%S.png -u" >>
       spawn "paplay /usr/share/sounds/freedesktop/stereo/screen-capture.oga"
      )

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusUp)
 
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusDown  )

    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp  )
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapUp  )
 
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapDown    )
 
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_f     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --restart")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myStartupHook  = -- I do not remember why this is here
                 setWMName "LG3D" >>
                 -- keyboard layout
                 spawn "setxkbmap us altgr-intl" >>
                 -- run fehbg to put a wallpaper
                 spawn "/home/jpgarcia/.fehbg" >>
                 -- on shaula xmonad had the ugly X cursor, lets put a decent one
                 spawn "xsetroot -cursor_name left_ptr" >>
                 spawn "compton -CGb --backend glx --xrender-sync \
                       \ --xrender-sync-fence -fcCz -l -17 -t -17 \
                       \ --config /home/jpgarcia/.xmonad/compton.conf">>
                 spawn "/home/jpgarcia/.screenlayout/aocedp.sh"
