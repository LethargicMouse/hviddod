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
import MySDL

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

($.) :: (Reader r1 :> es) => Eff (Reader r2 : es) a -> (r1 -> r2) -> Eff es a
m $. f = withR f m

withR :: (Reader r1 :> es) => (r1 -> r2) -> Eff (Reader r2 : es) a -> Eff es a
withR f = subsume . withReader f
