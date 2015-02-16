# lowgl

- Use GL with basic type safety.
- Use GL without bit fiddling and pointer wrangling.
- Documents the core (and only the core) workings of the hidden GL machine.
- Provides Haskell-language code examples of basic techniques.

## Install

```
cabal install lowgl
```

Uses the amazing [gl][1] package which brings the entirety of OpenGL to
haskell.

## Hello World

The hello world program shows a white triangle on a black background.
It uses the packages GLFW-b and monad-loops. Note that it forces a
3.2 core profile when setting up the context through GLFW.

```haskell
module Main where

import Control.Monad.Loops (whileM_)
import Data.Functor ((<$>))
import qualified Data.Vector.Storable as V

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Low

-- GLFW will be the shell of the demo
main = do
  GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  mwin <- GLFW.createWindow 640 480 "Hello World" Nothing Nothing
  case mwin of
    Nothing  -> putStrLn "createWindow failed"
    Just win -> do
      GLFW.makeContextCurrent (Just win)
      GLFW.swapInterval 1
      (vao, prog) <- setup -- load and configure objects
      whileM_ (not <$> GLFW.windowShouldClose win) $ do
        GLFW.pollEvents
        draw vao prog -- render
        GLFW.swapBuffers win

setup = do
  -- establish a VAO
  vao <- newVAO
  bindVAO vao
  -- load shader program
  vsource <- readFile "hello.vert"
  fsource <- readFile "hello.frag"
  prog <- newProgram vsource fsource
  useProgram prog
  -- load vertex data: three 2D vertex positions
  let blob = V.fromList
        [ -0.5, -0.5
        ,    0,  0.5
        ,  0.5, -0.5 ] :: V.Vector Float
  vbo <- newVBO blob StaticDraw
  bindVBO vbo
  -- connect program to vertex data via the VAO
  setVertexAttributeLayout [Attrib "position" 2 VFloat]
  return (vao, prog)

draw vao prog = do
  clearColorBuffer (0,0,0)
  bindVAO vao
  useProgram prog
  drawTriangles 3
```

The vertex shader file looks like


```glsl
#version 150

in vec2 position;

void main()
{
   gl_Position = vec4(position, 0.0, 1.0);
}
```

And the corresponding fragment shader file

```glsl
#version 150

out vec4 outColor;

void main()
{
  outColor = vec4(1.0, 1.0, 1.0, 1.0);
}
```

And the output should look like

![White triangle on black background]
(https://hackage.haskell.org/package/lowgl-0.2.1.1/docs/hello_world.png)


[1]: https://github.com/ekmett/gl/
