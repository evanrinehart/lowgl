{-# LANGUAGE RankNTypes #-}
module Graphics.GL.Low.Framebuffer (

-- | By default, rendering commands output graphics to the default framebuffer.
-- This includes the color buffer, the depth buffer, and the stencil buffer. It
-- is possible to render to a texture instead. This is important for many
-- techniques. Rendering to a texture (either color, depth, or depth/stencil)
-- is accomplished by using a framebuffer object (FBO).
--
-- The following ritual sets up an FBO with a blank 256x256 color texture for
-- off-screen rendering:
--
-- @
-- do
--   fbo <- newFBO
--   tex <- newEmptyTexture2D 256 256 :: IO (Tex2D RGB)
--   bindFramebuffer fbo
--   attachTex2D tex
--   bindFramebuffer DefaultFramebuffer
--   return (fbo, tex)
-- @
--
-- After binding an FBO to the framebuffer binding target, rendering commands
-- will output to its color attachment and possible depth/stencil attachment
-- if present. An FBO must have a color attachment before rendering. If only
-- the depth results are needed, then you can attach a color RBO instead of
-- a texture to the color attachment point.

  newFBO,
  bindFramebuffer,
  deleteFBO,
  attachTex2D,
  attachCubeMap,
  attachRBO,
  newRBO,
  deleteRBO,
  FBO,
  DefaultFramebuffer(..),
  RBO

  -- * Example
  -- $example
 
) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.IO.Class

import Data.Default
import Graphics.GL

import Graphics.GL.Low.Internal.Types
import Graphics.GL.Low.Classes
import Graphics.GL.Low.Common
import Graphics.GL.Low.Cube
import Graphics.GL.Low.Texture


-- | The default framebuffer.
data DefaultFramebuffer = DefaultFramebuffer deriving Show

instance Default DefaultFramebuffer where
  def = DefaultFramebuffer

instance Framebuffer DefaultFramebuffer where
  framebufferName _ = 0


-- | Binds an FBO or the default framebuffer to the framebuffer binding target.
-- Replaces the framebuffer already bound there.
bindFramebuffer :: (MonadIO m, Framebuffer a) => a -> m ()
bindFramebuffer x = glBindFramebuffer GL_FRAMEBUFFER (framebufferName x)

-- | Create a new framebuffer object. Before the framebuffer can be used for
-- rendering it must have a color image attachment.
newFBO :: (MonadIO m) => m FBO
newFBO = liftIO . fmap FBO $ alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)

-- | Delete an FBO.
deleteFBO :: (MonadIO m) => FBO -> m ()
deleteFBO (FBO n) = liftIO $ withArray [n] (\ptr -> glDeleteFramebuffers 1 ptr)

-- | Attach a 2D texture to the FBO currently bound to the
-- framebuffer binding target.
attachTex2D :: (MonadIO m, Attachable a) => Tex2D a -> m ()
attachTex2D tex =
  glFramebufferTexture2D
    GL_FRAMEBUFFER
    (attachPoint tex)
    GL_TEXTURE_2D
    (glObjectName tex)
    0

-- | Attach one of the sides of a cubemap texture to the FBO currently bound
-- to the framebuffer binding target.
attachCubeMap :: (MonadIO m, Attachable a) => CubeMap a -> Side -> m ()
attachCubeMap cm side =
  glFramebufferTexture2D
    GL_FRAMEBUFFER
    (attachPoint cm)
    (side cubeSideCodes)
    (glObjectName cm)
    0

-- | Attach an RBO to the FBO currently bound to the framebuffer binding
-- target.
attachRBO :: (MonadIO m, Attachable a) => RBO a -> m ()
attachRBO rbo = glFramebufferRenderbuffer
  GL_FRAMEBUFFER (attachPoint rbo) GL_RENDERBUFFER (unRBO rbo)

-- | Create a new renderbuffer with the specified dimensions.
newRBO :: (MonadIO m, InternalFormat a) => Int -> Int -> m (RBO a)
newRBO w h = do
  n <- liftIO $ alloca (\ptr -> glGenRenderbuffers 1 ptr >> peek ptr)
  rbo <- return (RBO n)
  glBindRenderbuffer GL_RENDERBUFFER n
  glRenderbufferStorage
    GL_RENDERBUFFER
    (internalFormat rbo)
    (fromIntegral w)
    (fromIntegral h)
  return rbo

-- | Delete an RBO.
deleteRBO :: (MonadIO m) => RBO a -> m ()
deleteRBO (RBO n) = liftIO $ withArray [n] (\ptr -> glDeleteRenderbuffers 1 ptr)


-- $example
--
-- <<framebuffer.gif Animated screenshot showing post-processing effect>>
--
-- This example program renders an animating object to an off-screen
-- framebuffer. The resulting texture is then shown on a full-screen quad
-- with an effect.
--
-- @
-- module Main where
-- 
-- import Control.Monad.Loops (whileM_)
-- import Data.Functor ((\<$\>))
-- import qualified Data.Vector.Storable as V
-- import Data.Maybe (fromJust)
-- import Data.Default
-- import Data.Word
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
--   mwin <- GLFW.createWindow 640 480 \"Framebuffer\" Nothing Nothing
--   case mwin of
--     Nothing  -> putStrLn "createWindow failed"
--     Just win -> do
--       GLFW.makeContextCurrent (Just win)
--       GLFW.swapInterval 1
--       (vao1, vao2, prog1, prog2, fbo, texture) <- setup
--       whileM_ (not <$> GLFW.windowShouldClose win) $ do
--         GLFW.pollEvents
--         t <- (realToFrac . fromJust) \<$\> GLFW.getTime
--         draw vao1 vao2 prog1 prog2 fbo texture t
--         GLFW.swapBuffers win
-- 
-- setup = do
--   -- primary subject
--   vao1 <- newVAO
--   bindVAO vao1
--   let blob = V.fromList
--         [ -0.5, -0.5, 0, 0
--         ,  0,    0.5, 0, 1
--         ,  0.5, -0.5, 1, 1] :: V.Vector Float
--   vbo <- newVBO blob StaticDraw
--   bindVBO vbo
--   vsource  <- readFile "framebuffer.vert"
--   fsource1 <- readFile "framebuffer1.frag"
--   prog1 <- newProgram vsource fsource1
--   useProgram prog1
--   setVertexLayout
--     [ Attrib "position" 2 GLFloat
--     , Attrib "texcoord" 2 GLFloat ]
-- 
--   -- full-screen quad to show the post-processed scene
--   vao2 <- newVAO
--   bindVAO vao2
--   let blob = V.fromList
--         [ -1, -1, 0, 0
--         , -1,  1, 0, 1
--         ,  1, -1, 1, 0
--         ,  1,  1, 1, 1] :: V.Vector Float
--   vbo <- newVBO blob StaticDraw
--   bindVBO vbo
--   indices <- newElementArray (V.fromList [0,1,2,3,2,1] :: V.Vector Word8) StaticDraw
--   bindElementArray indices
--   fsource2 <- readFile "framebuffer2.frag"
--   prog2 <- newProgram vsource fsource2
--   useProgram prog2
--   setVertexLayout
--     [ Attrib "position" 2 GLFloat
--     , Attrib "texcoord" 2 GLFloat ]
-- 
--   -- create an FBO to render the primary scene on
--   fbo <- newFBO
--   bindFramebuffer fbo
--   texture <- newEmptyTexture2D 640 480 :: IO (Tex2D RGB)
--   bindTexture2D texture
--   setTex2DFiltering Linear
--   attachTex2D texture
--   return (vao1, vao2, prog1, prog2, fbo, texture)
-- 
-- draw :: VAO -> VAO -> Program -> Program -> FBO -> Tex2D RGB -> Float -> IO ()
-- draw vao1 vao2 prog1 prog2 fbo texture t = do
--   -- render primary scene to fbo
--   bindVAO vao1
--   bindFramebuffer fbo
--   useProgram prog1
--   clearColorBuffer (0,0,0)
--   setUniform1f "time" [t]
--   drawTriangles 3
-- 
--   -- render results to quad on main screen
--   bindVAO vao2
--   bindFramebuffer DefaultFramebuffer
--   useProgram prog2
--   bindTexture2D texture
--   clearColorBuffer (0,0,0)
--   setUniform1f "time" [t]
--   drawIndexedTriangles 6 UByteIndices
-- @
--
-- The vertex shader for this program is
--
-- @
-- #version 150
-- in vec2 position;
-- in vec2 texcoord;
-- out vec2 Texcoord;
-- void main()
-- {
--     gl_Position = vec4(position, 0.0, 1.0);
--     Texcoord = texcoord;
-- }
-- @
--
-- The two fragment shaders, one for the object, one for the effect, are
--
-- @
-- #version 150
-- uniform float time;
-- in vec2 Texcoord;
-- out vec4 outColor;
-- void main()
-- {
--   float t = time;
--   outColor = vec4(
--     fract(Texcoord.x*5) < 0.5 ? sin(t*0.145) : cos(t*0.567),
--     fract(Texcoord.y*5) < 0.5 ? cos(t*0.534) : sin(t*0.321),
--     0.0, 1.0
--   );
-- }
-- @
--
-- @
-- #version 150
-- uniform float time;
-- uniform sampler2D tex;
-- in vec2 Texcoord;
-- out vec4 outColor;
-- 
-- void main()
-- {
--   float d = pow(10,(abs(cos(time))+1.5));
--   outColor c = texture(tex, floor(Texcoord*d)/d);
-- }
-- @
