{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Menu.Event
  ( MenuEvent (..),
    pollMenuEvents,
  )
where

import Data.Maybe
import Effectful
import MySDL
import SDL.Input.Keyboard.Codes

data MenuEvent
  = QuitMenuEvent
  | NewGameMenuEvent

pollMenuEvents :: (IOE :> es) => Eff es [MenuEvent]
pollMenuEvents = mapMaybe toMenuEvent <$> pollEvents

toMenuEvent :: Event -> Maybe MenuEvent
toMenuEvent (KeyPress key) = keyToMenuEvent key

keyToMenuEvent :: Keycode -> Maybe MenuEvent
keyToMenuEvent k = case k of
  KeycodeQ -> Just QuitMenuEvent
  KeycodeN -> Just NewGameMenuEvent
  KeycodeCapsLock -> Just QuitMenuEvent
  KeycodeEscape -> Just QuitMenuEvent
  _ -> Nothing
