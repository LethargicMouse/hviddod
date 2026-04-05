{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Menu
  ( runMenu,
  )
where

import Control.Monad
import Data.Maybe
import Effectful
import Effectful.Reader.Static
import MySDL

data MenuEvent
  = QuitMenu
  | RunNewGame

runMenu :: (IOE :> es, Reader Renderer :> es) => Eff es ()
runMenu = do
  lc <- updateMenu
  unless (isBreak lc) (drawMenu >> runMenu)

updateMenu :: (IOE :> es) => Eff es LoopControl
updateMenu = handleMenuEvents <$> pollMenuEvents

data LoopControl
  = Break
  | Continue

isBreak :: LoopControl -> Bool
isBreak Break = True
isBreak _ = False

pollMenuEvents :: (IOE :> es) => Eff es [MenuEvent]
pollMenuEvents = mapMaybe toMenuEvent <$> pollEvents

toMenuEvent :: Event -> Maybe MenuEvent
toMenuEvent (KeyPress key) = keyToMenuInput key

keyToMenuInput :: Keycode -> Maybe MenuEvent
keyToMenuInput k = case k of
  KeycodeQ -> Just QuitMenu
  KeycodeN -> Just RunNewGame
  KeycodeCapsLock -> Just QuitMenu
  KeycodeEscape -> Just QuitMenu
  _ -> Nothing

handleMenuEvents :: [MenuEvent] -> LoopControl
handleMenuEvents [] = Continue
handleMenuEvents (e : _) = case e of
  QuitMenu -> Break
  RunNewGame -> undefined

drawMenu :: (IOE :> es, Reader Renderer :> es) => Eff es ()
drawMenu = do
  clearBg Black
  display
