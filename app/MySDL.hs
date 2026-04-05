{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module MySDL
  ( Event (..),
    module SDL.Input.Keyboard.Codes,
    Renderer,
    pollEvents,
    clearBg,
    display,
    Color,
    black,
    white,
    initAll,
    createWindow,
    defaultWindow,
    createRenderer,
    defaultRenderer,
    destroyWindow,
    Texture,
    Rectangle,
    drawPicture,
    Picture (..),
    Font,
    Window,
    load,
    createText,
  )
where

import Data.Maybe
import Data.Text hiding (copy)
import Data.Word
import Effectful
import Effectful.Reader.Static
import Foreign.C.Types
import SDL hiding (Event, point, pollEvents)
import qualified SDL
import SDL.Font hiding (Color, height)
import qualified SDL.Font as Font hiding (Color)
import SDL.Input.Keyboard.Codes

type Color = V4 Word8

black, white :: Color
black = V4 0 0 0 255
white = V4 255 255 255 255

newtype Event
  = KeyPress Keycode

mkEvent :: SDL.Event -> Maybe Event
mkEvent e = case eventPayload e of
  KeyboardEvent ke ->
    if keyboardEventKeyMotion ke == Pressed
      then Just (KeyPress $ keysymKeycode $ keyboardEventKeysym ke)
      else Nothing
  _ -> Nothing

clearBg :: (Reader Renderer :> es, IOE :> es) => Color -> Eff es ()
clearBg color = do
  renderer <- ask
  rendererDrawColor renderer $= color
  clear renderer

display :: (Reader Renderer :> es, IOE :> es) => Eff es ()
display = present =<< ask

pollEvents :: (IOE :> es) => Eff es [Event]
pollEvents = mapMaybe mkEvent <$> SDL.pollEvents

initAll :: (IOE :> es) => Eff es ()
initAll = do
  initializeAll
  Font.initialize

drawPicture :: (Reader Renderer :> es, IOE :> es) => Picture -> Eff es ()
drawPicture (Picture texture rect) = do
  renderer <- ask
  copy renderer texture Nothing (Just rect)

data Picture
  = Picture Texture (Rectangle CInt)

textureFromSurface :: (IOE :> es, Reader Renderer :> es) => Surface -> Eff es Texture
textureFromSurface surface = do
  renderer <- ask
  createTextureFromSurface renderer surface

createText :: (Reader Renderer :> es, IOE :> es) => Font -> Text -> Point V2 CInt -> Eff es Picture
createText font text point = do
  surface <- solid font white text
  texture <- textureFromSurface surface
  (width, height) <- size font text
  freeSurface surface
  pure $
    Picture texture $
      Rectangle point $
        V2 (fromIntegral width) (fromIntegral height)
