{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad
import Data.Maybe
import Effectful
import Effectful.Reader.Static
import MySDL

data LoopControl
  = Break
  | Continue

data MenuEvent
  = QuitMenu
  | RunNewGame

isBreak :: LoopControl -> Bool
isBreak Break = True
isBreak _ = False

main :: IO ()
main = runEff $ do
  initializeAll
  window <- createWindow "hviddod" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  runMenu $$ renderer
  destroyWindow window

($$) :: Eff (Reader r : es) a -> r -> Eff es a
m $$ r = runReader r m

runMenu :: (IOE :> es, Reader Renderer :> es) => Eff es ()
runMenu = do
  lc <- updateMenu
  unless (isBreak lc) (drawMenu >> runMenu)

updateMenu :: (IOE :> es) => Eff es LoopControl
updateMenu = handleMenuEvents <$> pollMenuEvents

pollMenuEvents :: (IOE :> es) => Eff es [MenuEvent]
pollMenuEvents = mapMaybe toMenuEvent <$> pollEvents

toMenuEvent :: Event -> Maybe MenuEvent
toMenuEvent (KeyPress key) = keyToMenuInput key

keyToMenuInput :: Keycode -> Maybe MenuEvent
keyToMenuInput k = case k of
  KeycodeQ -> Just QuitMenu
  KeycodeN -> Just RunNewGame
  KeycodeCapsLock -> Just QuitMenu
  KeycodeEscape -> Just QuitMenu
  _ -> Nothing

handleMenuEvents :: [MenuEvent] -> LoopControl
handleMenuEvents [] = Continue
handleMenuEvents (e : _) = case e of
  QuitMenu -> Break
  RunNewGame -> undefined

drawMenu :: (IOE :> es, Reader Renderer :> es) => Eff es ()
drawMenu = do
  clearBg Black
  display
