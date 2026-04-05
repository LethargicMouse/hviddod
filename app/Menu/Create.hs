{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Menu.Create
  ( Menu (..),
    createMenu,
  )
where

import Control.Monad
import Data.Text hiding (index)
import Effectful
import Effectful.Reader.Static
import Foreign.C.Types
import MySDL
import SDL
import SDL.Font

data Menu
  = Menu
  { getButtons :: [Picture],
    getRenderer :: Renderer
  }

createMenu :: (IOE :> es, Reader Renderer :> es) => Eff es Menu
createMenu = do
  font <- load "font.ttf" 50
  buttons <- zipWithM (createButton font) [1 ..] ["[N]ew Game", "[Q]uit"]
  free font
  Menu buttons <$> ask

createButton :: (IOE :> es, Reader Renderer :> es) => Font -> CInt -> Text -> Eff es Picture
createButton font index text = createText font text (P $ V2 50 $ (index + 1) * 100)
