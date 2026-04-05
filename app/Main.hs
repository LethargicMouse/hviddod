{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad
import Data.Maybe
import Data.Word
import Effectful
import Effectful.Reader.Static
import SDL

data LoopControl
  = Break
  | Continue

newtype Input
  = KeyPress Keycode

data MenuInput
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
updateMenu = handleMenuEvents <$> pollEvents

handleMenuEvents :: [Event] -> LoopControl
handleMenuEvents = handleMenuInputs . mapMaybe (toMenuInput <=< toInput)

toMenuInput :: Input -> Maybe MenuInput
toMenuInput (KeyPress key) = keyToMenuInput key

keyToMenuInput :: Keycode -> Maybe MenuInput
keyToMenuInput k = case k of
  KeycodeQ -> Just QuitMenu
  KeycodeN -> Just RunNewGame
  KeycodeCapsLock -> Just QuitMenu
  KeycodeEscape -> Just QuitMenu
  _ -> Nothing

handleMenuInputs :: [MenuInput] -> LoopControl
handleMenuInputs [] = Continue
handleMenuInputs (e : _) = case e of
  QuitMenu -> Break
  RunNewGame -> undefined

toInput :: Event -> Maybe Input
toInput e = case eventPayload e of
  KeyboardEvent ke ->
    if keyboardEventKeyMotion ke == Pressed
      then Just (KeyPress $ keysymKeycode $ keyboardEventKeysym ke)
      else Nothing
  _ -> Nothing

drawMenu :: (IOE :> es, Reader Renderer :> es) => Eff es ()
drawMenu = do
  clearBg black
  display

display :: (Reader Renderer :> es, IOE :> es) => Eff es ()
display = present =<< ask

clearBg :: (Reader Renderer :> es, IOE :> es) => V4 Word8 -> Eff es ()
clearBg color = do
  renderer <- ask
  rendererDrawColor renderer $= color
  clear renderer

black :: V4 Word8
black = V4 0 0 0 255
