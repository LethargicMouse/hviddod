{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Effectful
import Effectful.Reader.Static
import Menu.Create
import Menu.Run
import MySDL

main :: IO ()
main = runEff $ do
  initAll
  window <- createWindow "hviddod" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  menu <- createMenu $$ renderer
  runMenu $$ menu >>= \case
    QuitMenu -> destroyWindow window
    RunNewGame -> undefined

($$) :: Eff (Reader r : es) a -> r -> Eff es a
m $$ r = runReader r m
