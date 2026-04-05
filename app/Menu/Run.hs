{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Menu.Run
  ( runMenu,
    AfterMenu (..),
  )
where

import Effectful
import Effectful.Reader.Static
import Menu.Create
import Menu.Event
import MyEffectful
import MySDL.Render

data LoopControl a
  = Break a
  | Continue

type MenuLoopControl = LoopControl AfterMenu

data AfterMenu
  = QuitMenu
  | RunNewGame

runMenu :: (IOE :> es, Reader Menu :> es) => Eff es AfterMenu
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

drawMenu :: (IOE :> es, Reader Menu :> es) => Eff es ()
drawMenu = do
  clearBg black $. getRenderer
  drawButtons
  display $. getRenderer

drawButtons :: (Reader Menu :> es, IOE :> es) => Eff es ()
drawButtons = mapM_ (withR getRenderer . drawPicture) =<< asks getButtons
