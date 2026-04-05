{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Effectful
import Menu.Create
import Menu.Run
import MyEffectful
import SDL
import qualified SDL.Font as Font

main :: IO ()
main = runEff $ do
  initializeAll
  Font.initialize
  window <- createWindow "hviddod" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  menu <- createMenu window $$ renderer
  run $$ menu
