{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------------------------
-- |
-- Module      :  Text.Tint.Colour
-- Maintainer  :  sturdyi12@mail.wlu.edu
-- Stability   :  experimental
-- Portability :  non-portable (LambdaCase, OverloadedStrings)
--
-- Various
---------------------------------------------------------------------------

module Text.Tint.Colour (
  -- * Types
    Colours(..)
  , DColour
  -- * Output functions
  , cssColours
  -- * Specific colour sets
  , lowContrastLight
  , solarizedLight
  ) where

import           Data.Colour.SRGB
import           Data.Monoid
import           Data.Text        hiding (tail)
import qualified Data.Text        as T

import           Text.Tint.Types

-- | Write out the css definitions for a set of colour definitions.
cssColours :: Colours -> Text
cssColours (Colours bgd mp) = ".tint {background-color:" <> toHex bgd
                              <> "; color: " <> toHex (mp NRM) <> "}\n"
                              <> (T.concat . fmap toEntry $ tail txtClasses)
  where toEntry cls = "pre.tint span." <> pack (show cls) <> " {color: "
                      <> toHex (mp cls) <> "}\n"
        toHex = pack . sRGB24show

-- Specific colour sets
-- Needs a bit of differentiation, and the TYP entry is not quite right.
-- | A generic low-contrast colour scheme.
lowContrastLight :: Colours
lowContrastLight = Colours {
    background = sRGB24read "#F0F0F0"
  , foreground = \c -> case c of
      NRM -> sRGB24read "#3A718B"
      FUN -> sRGB24read "#1C3372"
      CMT -> sRGB24read "#8C3A49"
      PRG -> sRGB24read "#8C3A49"
      STR -> sRGB24read "#A14D84"
      OP  -> sRGB24read "#965C42"
      KWD -> sRGB24read "#A14D84"
      TYP -> sRGB24read "#228b22"
      OT1 -> sRGB24read "#965C42"
      OT2 -> sRGB24read "#965C42"
  }

-- I do not know, I never use this.
solarizedLight :: Colours
solarizedLight = Colours {
    background = sRGB24read "#fdf6e3"
  , foreground = \c -> case c of
      NRM -> sRGB24read "#839496"
      FUN -> sRGB24read "#268bd2"
      CMT -> sRGB24read "#586e75"
      PRG -> sRGB24read "#cb4b16"
      STR -> sRGB24read "#2aa198"
      OP  -> sRGB24read "#6c71c4"
      KWD -> sRGB24read "#859900"
      TYP -> sRGB24read "#b58900"
      OT1 -> sRGB24read "#dc322f"
      OT2 -> sRGB24read "#d33682"
  }
