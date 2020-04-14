{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Emulator.CrtShader (activateCrtShader, deactivateCrtShader) where


import           Control.Monad
import           Data.ByteString hiding (putStrLn)
import           Data.StateVar
import           Graphics.Rendering.OpenGL.GL.Shaders
import           Text.RawString.QQ


crtShader = [r|

#version 430 core

out vec4 fColor;
void
main()
{
   fColor = vec4(0.0, 0.0, 1.0, 1.0);
}

|]


setupFragmentShader :: ByteString -> IO (Maybe Shader)
setupFragmentShader source = do
    shader <- createShader FragmentShader
    shaderSourceBS shader $= source
    compileShader shader
    success <- compileStatus shader
    when (not success) $ 
        shaderInfoLog shader >>= putStrLn
    let result = if success then Just else const Nothing
    return $ result shader


createProgramFrom :: Maybe Shader -> IO ()
createProgramFrom Nothing = pure ()
createProgramFrom (Just shader) = do
    program <- createProgram
    attachShader program shader
    linkProgram program
    linkSuccess <- linkStatus program
    if not linkSuccess 
    then programInfoLog program >>= putStrLn
    else do
        validateProgram program
        validateSuccess <- validateStatus program
        if not validateSuccess 
        then programInfoLog program >>= putStrLn
        else 
            currentProgram $= Just program


activateCrtShader :: IO ()
activateCrtShader = createProgramFrom =<< setupFragmentShader crtShader

deactivateCrtShader :: IO ()
deactivateCrtShader = currentProgram $= Nothing

