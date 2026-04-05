{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module MySDL
  ( Event (..),
    module SDL.Input.Keyboard.Codes,
    Renderer,
    pollEvents,
    clearBg,
    display,
    Color (..),
    initializeAll,
    createWindow,
    defaultWindow,
    createRenderer,
    defaultRenderer,
    destroyWindow,
  )
where

import Data.Maybe
import Data.Word
import Effectful
import Effectful.Reader.Static
import SDL hiding (Event, pollEvents)
import qualified SDL
import SDL.Input.Keyboard.Codes

data Color
  = Black

newtype Event
  = KeyPress Keycode

mkEvent :: SDL.Event -> Maybe Event
mkEvent e = case eventPayload e of
  KeyboardEvent ke ->
    if keyboardEventKeyMotion ke == Pressed
      then Just (KeyPress $ keysymKeycode $ keyboardEventKeysym ke)
      else Nothing
  _ -> Nothing

clearBg :: (Reader Renderer :> es, IOE :> es) => Color -> Eff es ()
clearBg color = do
  renderer <- ask
  rendererDrawColor renderer $= rawColor color
  clear renderer

rawColor :: Color -> V4 Word8
rawColor Black = V4 0 0 0 255

display :: (Reader Renderer :> es, IOE :> es) => Eff es ()
display = present =<< ask

pollEvents :: (IOE :> es) => Eff es [Event]
pollEvents = mapMaybe mkEvent <$> SDL.pollEvents
