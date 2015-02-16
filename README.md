# lowgl

Use modern but basic OpenGL from Haskell with this wrapper around the amazing
[gl][1] package. The gl package exposes the entire OpenGL API and even
auto-configures extensions available on your system. However it is a direct
translation of the C API, which means all the low level calls require a medium
amount of FFI negotiation. Worse than that is the amount of identifiers that
GL_ARE_FORMATTED_LIKE_THIS. Also, as is rightly pointed out, the OpenGL C API
has very little in the way of type safety. Besides safety, what's interesting
to me is the amount of guidance a minimal amount of types can provide to such a
complex interface. The square pegs go in the square holes! Even so, the
incredibly stateful semantics of OpenGL require a good bit of english to
accurately describe. Also it seems that 99% of OpenGL is strictly unnecessary
to achieve what's necessary for a cool game, demo, screensaver, or tutorial.
This package solves that by concentrating on a very small part of the whole
API. In short lowgl exists to

- use GL with basic type safety
- use GL without bit fiddling and pointer wrangling
- document the core (and only the core) workings of the hidden GL machine
- provide Haskell-language code examples of basic techniques
- clarify to me how modern GL works

## Install

```
cabal install lowgl
```

## Hello World

The hello world program shows a white triangle on a black background.
It uses the packages GLFW-b and monad-loops. Note that it forces a
3.2 core profile when setting up the context through GLFW.

```
module Main where

import Control.Monad.Loops (whileM_)
import Data.Functor ((\<$\>))
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
  mwin <- GLFW.createWindow 640 480 \"Hello World\" Nothing Nothing
  case mwin of
    Nothing  -> putStrLn "createWindow failed"
    Just win -> do
      GLFW.makeContextCurrent (Just win)
      GLFW.swapInterval 1
      (vao, prog) <- setup -- load and configure objects
      whileM_ (not \<$\> GLFW.windowShouldClose win) $ do
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


```
#version 150

in vec2 position;

void main()
{
   gl_Position = vec4(position, 0.0, 1.0);
}
```

And the corresponding fragment shader file

```
#version 150

out vec4 outColor;

void main()
{
  outColor = vec4(1.0, 1.0, 1.0, 1.0);
}
```

And the output should look like


[1]: https://github.com/ekmett/gl/
