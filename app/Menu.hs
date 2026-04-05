{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Menu
  ( runMenu,
    AfterMenu (..),
  )
where

import Data.Maybe
import Effectful
import Effectful.Reader.Static
import MySDL

data MenuEvent
  = QuitMenuEvent
  | NewGameMenuEvent

runMenu :: (IOE :> es, Reader Renderer :> es) => Eff es AfterMenu
runMenu =
  updateMenu >>= \case
    Continue -> drawMenu >> runMenu
    Break fm -> pure fm

updateMenu :: (IOE :> es) => Eff es MenuLoopControl
updateMenu = handleMenuEvents <$> pollMenuEvents

data LoopControl a
  = Break a
  | Continue

type MenuLoopControl = LoopControl AfterMenu

data AfterMenu
  = QuitMenu
  | RunNewGame

pollMenuEvents :: (IOE :> es) => Eff es [MenuEvent]
pollMenuEvents = mapMaybe toMenuEvent <$> pollEvents

toMenuEvent :: Event -> Maybe MenuEvent
toMenuEvent (KeyPress key) = keyToMenuInput key

keyToMenuInput :: Keycode -> Maybe MenuEvent
keyToMenuInput k = case k of
  KeycodeQ -> Just QuitMenuEvent
  KeycodeN -> Just NewGameMenuEvent
  KeycodeCapsLock -> Just QuitMenuEvent
  KeycodeEscape -> Just QuitMenuEvent
  _ -> Nothing

handleMenuEvents :: [MenuEvent] -> MenuLoopControl
handleMenuEvents [] = Continue
handleMenuEvents (e : _) = case e of
  QuitMenuEvent -> Break QuitMenu
  NewGameMenuEvent -> Break RunNewGame

drawMenu :: (IOE :> es, Reader Renderer :> es) => Eff es ()
drawMenu = do
  clearBg Black
  display
