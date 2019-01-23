-- This module provides some mnemonic names for keysyms of keys in
-- the ergonomic keyboard, model 4000
-- (The brand by is an unmentionable corporation that also developed
-- a non unix operating system sometimes useful for gaming)
-- I bought an OEM unit, of course



module Kb4000 where

import XMonad
import Data.Word

-- | prefix used by most keys
pref :: Word64
pref = 0x1008ff00

xK_WebHome :: KeySym
xK_WebHome = 0x18   + pref

xK_Search :: KeySym
xK_Search  = 0x1b   + pref

xK_Mail :: KeySym
xK_Mail = 0x19      + pref

xK_Fav1 :: KeySym
xK_Fav1 = 0x45      + pref

xK_Fav2 :: KeySym
xK_Fav2 = 0x46      + pref

xK_Fav3 :: KeySym
xK_Fav3 = 0x47      + pref

xK_Fav4 :: KeySym
xK_Fav4 = 0x48      + pref

xK_Fav5 :: KeySym
xK_Fav5 = 0x49      + pref

xK_FavStar :: KeySym
xK_FavStar = 0x30   + pref

xK_VolDown :: KeySym
xK_VolDown = 0x11   + pref

xK_VolUp :: KeySym
xK_VolUp   = 0x13   + pref

xK_VolMute :: KeySym
xK_VolMute = 0x12   + pref

xK_PlayPause :: KeySym
xK_PlayPause = 0x14 + pref

xK_Calculator :: KeySym
xK_Calculator = 0x1d + pref

xK_ThumbFw :: KeySym
xK_ThumbFw = 0x27    + pref

xK_ThumbBk :: KeySym
xK_ThumbBk = 0x26    + pref


xK_ZoomPlus :: KeySym
xK_ZoomPlus = 0xff52

xK_ZoomMinus :: KeySym
xK_ZoomMinus = 0xff54
