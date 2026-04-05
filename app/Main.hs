{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Effectful
import Effectful.Reader.Static
import Menu
import MySDL

main :: IO ()
main = runEff $ do
  initializeAll
  window <- createWindow "hviddod" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  runMenu $$ renderer >>= \case
    QuitMenu -> destroyWindow window
    RunNewGame -> undefined

($$) :: Eff (Reader r : es) a -> r -> Eff es a
m $$ r = runReader r m
