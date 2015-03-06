module Graphics.GL.Low.Blending (

-- | When blending is enabled, colors written to the color buffer will be
-- blended using a formula with the color already there. The three options
-- for the formula are:
--
-- - S*s + D*d ('FuncAdd', the default)
-- - S*s - D*d ('FuncSubtract')
-- - D*d - S*s ('FuncReverseSubtract')
--
-- where S and D are source and destination color components respectively. The
-- factors s and d are computed blending factors which can depend on the alpha
-- component of the source pixel, the destination pixel, or a specified
-- constant color. See 'basicBlending' for a common choice.

  enableBlending,
  disableBlending,
  noBlending,
  basicBlending,
  premultipliedBlending,
  Blending(..),
  BlendFactor(..),
  BlendEquation(..)

  -- * Example
  -- $example
) where

import Control.Monad.IO.Class
import Graphics.GL

import Graphics.GL.Low.Classes
import Graphics.GL.Low.Internal.Types

-- | Blending parameters.
data Blending = Blending
  { sFactor :: BlendFactor
  , dFactor :: BlendFactor
  , blendFunc :: BlendEquation
  , blendColor :: (Float,Float,Float,Float) }

-- | Enable blending with the specified blending parameters.
enableBlending :: (MonadIO m) => Blending -> m ()
enableBlending (Blending s d f (r,g,b,a)) = do
  glBlendFunc (toGL s) (toGL d)
  glBlendEquation (toGL f)
  let c = realToFrac
  glBlendColor (c r) (c g) (c b) (c a)
  glEnable GL_BLEND

-- | Disable alpha blending.
disableBlending :: (MonadIO m) => m ()
disableBlending = glDisable GL_BLEND

-- | This blending configuration is suitable for ordinary alpha blending
-- transparency effects.
--
-- @
-- Blending
--   { sFactor   = BlendSourceAlpha
--   , dFactor   = BlendOneMinusSourceAlpha
--   , blendFunc = FuncAdd }
-- @
basicBlending :: Blending
basicBlending = noBlending
  { sFactor = BlendSourceAlpha
  , dFactor = BlendOneMinusSourceAlpha }

-- | This configuration produces no blending effect.
noBlending = Blending
    { sFactor = BlendOne
    , dFactor = BlendZero
    , blendFunc = FuncAdd
    , blendColor = (0,0,0,0) }

-- | This blending configuration is suitable for premultiplied alpha blending.
--
-- @
-- Blending
--   { sFactor   = BlendOne
--   , dFactor   = BlendOneMinusSourceAlpha
--   , blendFunc = FuncAdd }
-- @
premultipliedBlending :: Blending
premultipliedBlending = noBlending
    { sFactor = BlendOne
    , dFactor = BlendOneMinusSourceAlpha }


-- $example
--
-- <<blending1.png Blending Before>> <<blending2.png Blending After>>
--
-- This program draws two half-transparent shapes. When you press a key they
-- are rendered in the opposite order. This makes one appear as if it were in
-- front of the other. Because the depth test (see "Graphics.GL.Low.Depth")
-- must be disabled while using this kind of blending, there may be significant
-- overdraw in areas with many blending layers. This can harm performance.
-- Also the order-dependency can make using alpha blending in a 3D scene
-- complex or impossible. It may make more sense to use an off-screen render
-- pass (see "Graphics.GL.Low.Framebuffer") and an appropriate shader to
-- simulate transparency effects.
--
-- @
-- module Main where
-- 
-- import Control.Monad.Loops (whileM_)
-- import Data.Functor ((\<$\>))
-- import qualified Data.Vector.Storable as V
-- import Control.Concurrent.STM
-- 
-- import qualified Graphics.UI.GLFW as GLFW
-- import Linear
-- import Graphics.GL.Low
-- 
-- main = do
--   GLFW.init
--   GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
--   GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
--   GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
--   GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
--   mwin <- GLFW.createWindow 640 480 \"Blending\" Nothing Nothing
--   case mwin of
--     Nothing  -> putStrLn "createWindow failed"
--     Just win -> do
--       GLFW.makeContextCurrent (Just win)
--       GLFW.swapInterval 1
--       shouldSwap <- newTVarIO False
--       (GLFW.setKeyCallback win . Just)
--         (\_ _ _ _ _ -> atomically (modifyTVar shouldSwap not))
--       (vao, prog) <- setup
--       whileM_ (not \<$\> GLFW.windowShouldClose win) $ do
--         GLFW.pollEvents
--         draw vao prog shouldSwap
--         GLFW.swapBuffers win
-- 
-- setup = do
--   vao <- newVAO
--   bindVAO vao
--   vsource <- readFile "blending.vert"
--   fsource <- readFile "blending.frag"
--   prog <- newProgram vsource fsource
--   useProgram prog
--   let blob = V.fromList
--         [ -0.5,  0.5
--         ,  0.5,    0
--         , -0.5, -0.5 ] :: V.Vector Float
--   vbo <- newVBO blob StaticDraw
--   bindVBO vbo
--   setVertexLayout [Attrib "position" 2 GLFloat]
--   enableBlending basicBlending
--   return (vao, prog)
-- 
-- draw vao prog shouldSwap = do
--   clearColorBuffer (0,0,0)
--   yes <- readTVarIO shouldSwap
--   if yes
--     then sequence [drawRed, drawGreen]
--     else sequence [drawGreen, drawRed]
-- 
-- drawGreen = do
--   setUniform3f "color" [V3 0 1 0]
--   setUniform1f "alpha" [0.5]
--   setUniform44 "move" [eye4]
--   drawTriangles 3
-- 
-- drawRed = do
--   let ninety = pi/2
--   let move = mkTransformation (axisAngle (V3 0 0 1) ninety) (V3 0.25 0.5 0)
--   setUniform3f "color" [V3 1 0 0]
--   setUniform1f "alpha" [0.5]
--   setUniform44 "move" [transpose move]
--   drawTriangles 3
-- @
-- 
-- blending.vert
--
-- @
-- #version 150
-- 
-- in vec3 Color;
-- in float Alpha;
-- out vec4 outColor;
-- 
-- void main()
-- {
--     outColor = vec4(Color, Alpha);
-- }
-- @
--
-- blending.frag
--
-- @
-- #version 150
-- 
-- uniform vec3 color;
-- uniform float alpha;
-- uniform mat4 move;
-- 
-- in vec2 position;
-- out vec3 Color;
-- out float Alpha;
-- 
-- void main()
-- {
--     gl_Position = move * vec4(position, 0.0, 1.0);
--     Color = color;
--     Alpha = alpha;
-- }
-- @
