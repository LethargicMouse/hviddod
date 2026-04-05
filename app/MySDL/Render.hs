{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module MySDL.Render
  ( clearBg,
    display,
    Color,
    black,
    white,
    drawPicture,
    Picture (..),
    createText,
  )
where

import Data.Text hiding (copy)
import Data.Word
import Effectful
import Effectful.Reader.Static
import Foreign.C.Types
import SDL hiding (point)
import SDL.Font hiding (Color, height)

type Color = V4 Word8

black, white :: Color
black = V4 0 0 0 255
white = V4 255 255 255 255

clearBg :: (Reader Renderer :> es, IOE :> es) => Color -> Eff es ()
clearBg color = do
  renderer <- ask
  rendererDrawColor renderer $= color
  clear renderer

display :: (Reader Renderer :> es, IOE :> es) => Eff es ()
display = present =<< ask

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
