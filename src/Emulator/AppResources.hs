module Emulator.AppResources where

import           Data.IORef
import           Graphics.Rendering.OpenGL as OpenGL
import           SDL
import           Communication
import           Emulator.JoyControls
import           Nes.Emulation.Monad


data OpenGLResources = OpenGLResources {
  glContext       :: GLContext,
  crtProgram      :: Program,
  textureUniform  :: UniformLocation
}


-- | Window resources
data AppResources = AppResources {
  window          :: Window,
  renderer        :: Renderer,
  screen          :: Texture,
  openGLResources :: Maybe OpenGLResources,
  commRes         :: CommResources,
  nes             :: IORef Nes,
  reset           :: IORef Bool,
  reboot          :: IORef Bool,
  continousMode   :: IORef Bool,
  joyIsSecondCtrl :: IORef Bool,
  joys            :: IORef JoyControlState,
  saveFolder      :: IORef (Maybe FilePath),
  fullscreen      :: IORef Bool,
  useCrtShader    :: IORef Bool
}