{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Menu.Run (run) where

import Effectful
import Effectful.Reader.Static
import Menu.Create
import Menu.Event
import MyEffectful
import MySDL.Render
import SDL

data Action
  = QuitMenu
  | RunNewGame
  | Continue

run :: (IOE :> es, Reader Menu :> es) => Eff es ()
run =
  updateMenu >>= \case
    Continue -> drawMenu >> run
    QuitMenu -> destroyWindow =<< asks getWindow
    RunNewGame -> undefined

updateMenu :: (IOE :> es) => Eff es Action
updateMenu = handleMenuEvents <$> pollMenuEvents

handleMenuEvents :: [MenuEvent] -> Action
handleMenuEvents [] = Continue
handleMenuEvents (e : _) = case e of
  QuitMenuEvent -> QuitMenu
  NewGameMenuEvent -> RunNewGame

drawMenu :: (IOE :> es, Reader Menu :> es) => Eff es ()
drawMenu = do
  clearBg black $. getRenderer
  drawButtons
  display $. getRenderer

drawButtons :: (Reader Menu :> es, IOE :> es) => Eff es ()
drawButtons = mapM_ (withR getRenderer . drawPicture) =<< asks getButtons
