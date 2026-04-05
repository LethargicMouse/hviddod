{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module MySDL.Event
  ( Event (..),
    pollEvents,
  )
where

import Data.Maybe
import Effectful
import SDL hiding (Event, pollEvents)
import qualified SDL

newtype Event
  = KeyPress Keycode

mkEvent :: SDL.Event -> Maybe Event
mkEvent e = case eventPayload e of
  KeyboardEvent ke ->
    if keyboardEventKeyMotion ke == Pressed
      then Just (KeyPress $ keysymKeycode $ keyboardEventKeysym ke)
      else Nothing
  _ -> Nothing

pollEvents :: (IOE :> es) => Eff es [Event]
pollEvents = mapMaybe mkEvent <$> SDL.pollEvents
