{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Menu.Run
  ( runMenu,
    AfterMenu (..),
    createMenu,
  )
where

import Control.Monad
import Data.Text hiding (index)
import Effectful
import Effectful.Reader.Static
import Foreign.C.Types
import Menu.Event
import MySDL
import SDL.Font hiding (height)
import SDL.Vect hiding (point)
import SDL.Video.Renderer

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

($.) :: (Reader r1 :> es) => Eff (Reader r2 : es) a -> (r1 -> r2) -> Eff es a
m $. f = withR f m

withR :: (Reader r1 :> es) => (r1 -> r2) -> Eff (Reader r2 : es) a -> Eff es a
withR f = subsume . withReader f

data Menu
  = Menu
  { getButtons :: [Picture],
    getRenderer :: Renderer
  }

drawButtons :: (Reader Menu :> es, IOE :> es) => Eff es ()
drawButtons = mapM_ (withR getRenderer . drawPicture) =<< asks getButtons

data LoopControl a
  = Break a
  | Continue

type MenuLoopControl = LoopControl AfterMenu

data AfterMenu
  = QuitMenu
  | RunNewGame

createMenu :: (IOE :> es, Reader Renderer :> es) => Eff es Menu
createMenu = do
  font <- load "font.ttf" 50
  buttons <- zipWithM (createButton font) [1 ..] ["[N]ew Game", "[Q]uit"]
  free font
  Menu buttons <$> ask

createButton :: (IOE :> es, Reader Renderer :> es) => Font -> CInt -> Text -> Eff es Picture
createButton font index text = createText font text (P $ V2 50 $ (index + 1) * 100)
