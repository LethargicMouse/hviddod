{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Effectful
import Menu.Create
import Menu.Run
import MyEffectful
import MySDL
import SDL

main :: IO ()
main = runEff $ do
  initAll
  window <- createWindow "hviddod" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  menu <- createMenu $$ renderer
  runMenu $$ menu >>= \case
    QuitMenu -> destroyWindow window
    RunNewGame -> undefined
