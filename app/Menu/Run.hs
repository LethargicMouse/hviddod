{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Menu.Run
  ( runMenu,
    AfterMenu (..),
    pollMenuEvents,
  )
where

import Effectful
import Effectful.Reader.Static
import Menu.Event
import MySDL

runMenu :: (IOE :> es, Reader Renderer :> es) => Eff es AfterMenu
runMenu =
  updateMenu >>= \case
    Continue -> drawMenu >> runMenu
    Break fm -> pure fm

updateMenu :: (IOE :> es) => Eff es MenuLoopControl
updateMenu = handleMenuEvents <$> pollMenuEvents

handleMenuEvents :: [MenuEvent] -> MenuLoopControl
handleMenuEvents [] = Continue
handleMenuEvents (e : _) = case e of
  QuitMenuEvent -> Break QuitMenu
  NewGameMenuEvent -> Break RunNewGame

drawMenu :: (IOE :> es, Reader Renderer :> es) => Eff es ()
drawMenu = do
  clearBg Black
  display

data LoopControl a
  = Break a
  | Continue

type MenuLoopControl = LoopControl AfterMenu

data AfterMenu
  = QuitMenu
  | RunNewGame
