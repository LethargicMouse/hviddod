{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

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
  menu <- createMenu $$ renderer
  runMenu window menu

runMenu :: (IOE :> es) => Window -> Menu -> Eff es ()
runMenu window menu =
  run $$ menu >>= \case
    QuitMenu -> destroyWindow window
    RunNewGame -> undefined
