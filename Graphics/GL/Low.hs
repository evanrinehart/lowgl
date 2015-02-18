-- | Basic low-level GL wrapper and reference.
module Graphics.GL.Low (

  -- * Overview
  -- | OpenGL is a graphics rendering interface. This library exposes a vastly
  -- simplified subset of OpenGL that is hopefully still complete enough for
  -- many purposes, such as following tutorials, making simple games, and
  -- demos. In particular the intention is to concentrate on a subset of
  -- OpenGL 3.2 (Core Profile) roughly corresponding to ES 2.0.
  --
  -- A second primary purpose is to document the complex model behind the
  -- interface in a way that is more elaborate than tutorials and more concise
  -- than the spec. As such, this is an experimental project to aid my own
  -- process of understanding OpenGL. It seems that understanding the entire
  -- picture up-front is the only way to get started, so this should also serve
  -- as a quick reference guide to the core commands and concepts.
  --
  -- This library uses the `gl' package for raw bindings to OpenGL and the
  -- `linear' package for matrices.

  -- * Example
  --
  -- | The hello world program shows a white triangle on a black background.
  -- It uses the packages `GLFW-b' and `monad-loops'. Note that it forces a
  -- 3.2 core profile when setting up the context through GLFW.
  --
  -- @
  -- module Main where
  --
  -- import Control.Monad.Loops (whileM_)
  -- import Data.Functor ((\<$\>))
  -- import qualified Data.Vector.Storable as V
  -- 
  -- import qualified Graphics.UI.GLFW as GLFW
  -- import Graphics.GL.Low
  -- 
  -- -- GLFW will be the shell of the demo
  -- main = do
  --   GLFW.init
  --   GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  --   GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
  --   GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
  --   GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  --   mwin <- GLFW.createWindow 640 480 \"Hello World\" Nothing Nothing
  --   case mwin of
  --     Nothing  -> putStrLn "createWindow failed"
  --     Just win -> do
  --       GLFW.makeContextCurrent (Just win)
  --       GLFW.swapInterval 1
  --       (vao, prog) <- setup -- load and configure objects
  --       whileM_ (not \<$\> GLFW.windowShouldClose win) $ do
  --         GLFW.pollEvents
  --         draw vao prog -- render
  --         GLFW.swapBuffers win
  -- 
  -- setup = do
  --   -- establish a VAO
  --   vao <- newVAO
  --   bindVAO vao
  --   -- load shader program
  --   vsource <- readFile "hello.vert"
  --   fsource <- readFile "hello.frag"
  --   prog <- newProgram vsource fsource
  --   useProgram prog
  --   -- load vertex data: three 2D vertex positions
  --   let blob = V.fromList
  --         [ -0.5, -0.5
  --         ,    0,  0.5
  --         ,  0.5, -0.5 ] :: V.Vector Float
  --   vbo <- newVBO blob StaticDraw
  --   bindVBO vbo
  --   -- connect program to vertex data via the VAO
  --   setVertexLayout [Attrib "position" 2 GLFloat]
  --   return (vao, prog)
  -- 
  -- draw vao prog = do
  --   clearColorBuffer (0,0,0)
  --   bindVAO vao
  --   useProgram prog
  --   drawTriangles 3
  -- @
  --
  -- The vertex shader file looks like
  --
  -- @
  -- #version 150
  --
  -- in vec2 position;
  --
  -- void main()
  -- {
  --    gl_Position = vec4(position, 0.0, 1.0);
  -- }
  -- @
  --
  -- And the corresponding fragment shader file
  --
  -- @
  -- #version 150
  --
  -- out vec4 outColor;
  --
  -- void main()
  -- {
  --   outColor = vec4(1.0, 1.0, 1.0, 1.0);
  -- }
  -- @
  --
  -- And the output should look like
  --
  -- <<hello_world.png Hello World>>

  -- * OpenGL API Basically
  --
  -- | <https://www.opengl.org/registry/doc/glspec32.core.20090803.pdf The spec>
  -- for OpenGL 3.2 is actually quite readable and is worth reviewing.
  -- The following is my synopsis of things which roughly coincide with the
  -- simplified OpenGL ES 2.0.

  -- ** Objects
  -- | Objects may be created and destroyed by client code. They include:
  --
  -- - Vertex Array Object ('VAO')
  -- - Buffer Objects ('VBO', 'ElementArray')
  -- - Textures ('Tex2D', 'CubeMap')
  -- - Shader 'Program's
  -- - Framebuffer Objects ('FBO')
  -- - Renderbuffer Objects ('RBO')

  -- ** Binding Targets
  -- | Objects are referenced with integers (called names in GL), so binding
  -- targets can be thought of as global variables to put those references.
  -- Many operations implicitly read from these globals to determine what the
  -- target object of the operation is. They include:
  --
  -- - Vertex array binding target (for VAO)
  -- - Buffer binding targets (ARRAY_BUFFER and ELEMENT_ARRAY_BUFFER)
  -- - Texture binding targets (TEXTURE_2D and TEXTURE_CUBE_MAP)
  -- - Framebuffer binding target (for FBO)
  --
  -- (not binding targets but similar)
  --
  -- - Shader program "in use"
  -- - Texture units
  -- - Current active texture unit
  -- - Image attachment points of an FBO

  -- ** Shader Programs
  -- | See "Graphics.GL.Low.Shader"

  -- ** VAO
  -- | The VAO is essential. At least one VAO must be created and bound to the
  -- vertex array binding target before rendering, before configuring a
  -- program's vertex attributes. Here is why: the VAO stores the association
  -- between vertex inputs in the program and a VBO from which to pipe input
  -- from. It also stores the format of the VBO data, which is otherwise just
  -- a big blob. Finally, the VAO stores the state of the element array binding
  -- target used for indexed rendering.
  --
  -- After installing a program with 'useProgram' and binding a source VBO
  -- to the array buffer binding target ('bindVBO') then the bound VAO can be
  -- updated ('setVertexLayout') with new vertex attribute information.
  -- After this, the VBO can be rebound to configure a different set of inputs
  -- with a different source. Many VAOs can be created and swapped out to pipe
  -- vertex data in different ways to different programs (or the same program).
  --
  -- When a VAO is bound ('bindVAO') it restores the state of the element array
  -- binding target. For this reason you can think of that binding target as
  -- simply being a function of the VAO itself rather than a separate global
  -- state.

  -- ** Uniforms and Samplers (Textures)
  -- | Programs may also have uniform variables and "sampler uniforms" as
  -- input. Uniforms are accessible from the vertex or fragment shader part of
  -- the program but their values are fixed during the course of a rendering
  -- command. They can be set and reset with the setUniform family (ex.
  -- 'setUniform1f'), which updates the current program object with new uniform
  -- values. Among other things, updating the uniforms each frame is the main
  -- way to animate a scene.
  --
  -- Samplers are textures that the shader can interpolate to get "in between"
  -- values. The texture a sampler uses is determined by the contents of the
  -- texture unit that that sampler points to. The sampler is a uniform with
  -- an integer type. This integer is the texture unit to use. The word texture
  -- should not be construed to mean a color image. Shaders can make use of
  -- many kinds of multi-dimensional data that happen to be available through
  -- the samplers.

  -- ** Texture Objects and Texture Units
  -- | Before a shader can use a texture it must be assigned to a texture unit.
  -- First set the active texture unit to the desired unit number
  -- ('setActiveTextureUnit') then bind the texture object to one of the
  -- two texture binding targets, depending on what kind of texture it is (2D
  -- or cubemap). Binding a texture has the side effect of assigning it to the
  -- active texture unit.

  -- ** Custom Framebuffers
  -- | It is possible (and important in many techniques) to utilize an
  -- off-screen render target. To do this create an FBO ('newFBO'), bind it to
  -- the framebuffer binding target ('bindFramebuffer') and attach a color
  -- /image/ object (texture or renderbuffer object). If necessary a depth
  -- image or combination depth-stencil image can be attached as well. If no
  -- color image is attached then the FBO is incomplete and rendering will be
  -- an error.  After rendering to an FBO any textures that were attached can
  -- be used in a second pass by assigning them to a texture unit. Watch out
  -- for feedback loops accidentally sampling a texture that is also being
  -- rendered to at the same time!
  --
  -- A renderbuffer object is a minor character to be used when you do not
  -- expect to use the results of rendering but need an image anyway. For
  -- example you may need a depth buffer to do depth testing, or you may want
  -- to ignore the (required for rendering to work at all) color buffer.

  -- ** Images and Image Formats
  -- | FBOs have attachment points for /images/. A texture serves as an image
  -- and a renderbuffer object serves as an image. Images have an "internal
  -- format" which describes the size and interpretation of pixel components.
  -- There are seven internal formats, five of which are color image formats
  -- such as grayscale and RGB. The other two are the depth buffer format and
  -- the combination depth-stencil format. RBOs ('newRBO') and empty textures
  -- ('newEmptyTexture2D', 'newEmptyCubeMap') can be created with any of these
  -- formats.
  --
  -- (The above is a gross simplification of OpenGL's image formats. I should
  -- probably revise, because it may greatly improve performance to use some
  -- of the 16-bit color formats rather than 32. Also HDR color format.)

  -- ** Depth Testing and Stencil Testing
  -- | The depth test and stencil test use extra buffers in parallel with the
  -- color buffer to cause regions of pixels to not show. It does this by
  -- making a comparison between the depth each pixel and the value present
  -- in those buffers, then updating the buffers as necessary. The stencil
  -- test in particular has many configurable options. See the respective
  -- modules for the "Graphics.GL.Low.Depth" and "Graphics.GL.Low.Stencil"
  -- tests. 

  -- ** Scissor Test
  -- | The scissor test, if enabled ('enableScissorTest'), disallows all
  -- rendering outside of a rectangle region of the window called the scissor
  -- box.

  -- ** Coordinate Systems (Mappings)
  -- | There are three transformation mechanisms which work together to get raw
  -- vertex data from VBOs to rasterized primitives somewhere on the window.
  -- You can imagine four coordinate systems between these three transformations
  -- if you want to.
  --
  -- - The __vertex shader__ takes vertex positions as specified in vertex
  -- attributes to clip space. This is how the client code specifies a camera,
  -- movement of objects, and perspective.
  -- - The __perspective division__ or ""W-divide"" takes vertices from clip
  -- space and maps them to normalized device coordinates (NDC) by dividing all
  -- the components of the vertex by that vertex's W component. This allows a
  -- perspective effect to be accomplished in the shader by modifying the W
  -- components. You can't configure this W-division; it just happens.  Note
  -- that if W = 1 for all vertices then this step has no effect. This is
  -- useful for orthographic projections. The resulting geometry will be
  -- clipped to a 2x2x2 cube centered around the origin. You can think of an XY
  -- plane of this cube as the viewport of the final 2D image.
  -- - The configurable __viewport transformation__ ('setViewport') will then
  -- position the viewport somewhere in the window.  This step is necessary
  -- because your window is probably not a 2x2 square.  The viewport
  -- transformation is configured by specifying a rectangular region of your
  -- window where you want the image to map to. The default setting for this is
  -- to fill the entire window with the viewport.  If you didn't previously
  -- account for your aspect ratio then this will have the effect of squishing
  -- the scene, so you need to compensate in the vertex shader.

  -- ** Rendering Points, Lines, and Triangles
  -- | The draw family (ex. 'drawTriangles') of commands commissions the
  -- rendering of a certain number of vertices worth of primitives. The
  -- current program will get input from the current VAO, the current texture
  -- units, and execute on all the potentially affected pixels in the current
  -- framebuffer. Vertexes are consumed in the order they appear in their
  -- respective source VBOs. If the VAO is missing, the program is missing, or
  -- the current framebuffer has no color attachment, then rendering will not
  -- work.
  --
  -- The drawIndexed family (ex. 'drawIndexedTriangles') of commands carries
  -- out the same effects as the non-indexed rendering commands but traverses
  -- vertices in an order determined by the sequence of indexes packed in the
  -- ElementArray currently bound to the element array binding target. This
  -- mainly allows a huge reuse of vertex data in the case that the object
  -- being rendered forms a closed mesh.


  -- * VAO
  -- | See also "Graphics.GL.Low.VAO"
  newVAO,
  bindVAO,
  deleteVAO,
  VAO,

  -- * Buffer Objects
  -- | See also "Graphics.GL.Low.BufferObject"
  newVBO,
  newElementArray,
  bindVBO,
  bindElementArray,
  updateVBO,
  updateElementArray,
  deleteBufferObject,
  VBO,
  ElementArray,
  UsageHint(..),

  -- * Shader Program
  -- | See also "Graphics.GL.Low.Shader"
  newProgram,
  newProgramSafe,
  useProgram,
  deleteProgram,
  setUniform1f, 
  setUniform2f,
  setUniform3f,
  setUniform4f,
  setUniform1i,
  setUniform2i,
  setUniform3i,
  setUniform4i,
  setUniform22,
  setUniform33,
  setUniform44,
  Program,
  ProgramError(..),

  -- ** Vertex Attributes
  -- | See also "Graphics.GL.Low.VertexAttrib"
  setVertexLayout,
  VertexLayout(..),
  DataType(..),


  -- * Textures
  -- | See also "Graphics.GL.Low.Texture"
  newTexture2D,
  newCubeMap,
  newEmptyTexture2D,
  newEmptyCubeMap,
  deleteTexture,
  setActiveTextureUnit,
  bindTexture2D,
  bindTextureCubeMap,
  setTex2DFiltering,
  setCubeMapFiltering,
  setTex2DWrapping,
  setCubeMapWrapping,
  Tex2D,
  CubeMap,
  Dimensions(..),
  Cube(..),
  Side,
  Filtering(..),
  Wrapping(..),

  -- * Rendering
  --
  -- ** Primitives
  -- | See also "Graphics.GL.Low.Render"
  drawPoints,
  drawLines,
  drawLineStrip,
  drawLineLoop,
  drawTriangles,
  drawTriangleStrip,
  drawTriangleFan,
  drawIndexedPoints,
  drawIndexedLines,
  drawIndexedLineStrip,
  drawIndexedLineLoop,
  drawIndexedTriangles,
  drawIndexedTriangleStrip,
  drawIndexedTriangleFan,
  setViewport,
  enableScissorTest,
  disableScissorTest,
  enableCulling,
  disableCulling,
  Viewport(..),
  Culling(..),
  IndexFormat(..),

  -- ** Color Buffer
  -- | See also "Graphics.GL.Low.Color"
  enableColorWriting,
  disableColorWriting,
  clearColorBuffer,

  -- ** Depth Test
  -- | See also "Graphics.GL.Low.Depth"
  enableDepthTest,
  disableDepthTest,
  clearDepthBuffer,

  -- ** Stencil Test
  -- | See also "Graphics.GL.Low.Stencil".
  enableStencil,
  disableStencil,
  clearStencilBuffer,
  basicStencil,
  Stencil(..),
  StencilFunc(..),
  StencilOp(..),

  -- ** Blending
  -- | See also "Graphics.GL.Low.Blending".

  enableBlending,
  disableBlending,
  basicBlending,
  Blending(..),
  BlendFactor(..),
  BlendEquation(..),

  -- * Framebuffers
  -- | See also "Graphics.GL.Low.Framebuffer"
  DefaultFramebuffer(..),
  FBO,
  bindFramebuffer,
  newFBO,
  attachTex2D,
  attachCubeMap,
  attachRBO,
  deleteFBO,

  -- * Renderbuffers
  RBO,
  newRBO,
  deleteRBO,

  -- * Errors
  GLError(..),
  getGLError,
  assertNoGLError,

  -- * Image Formats
  Alpha,
  Luminance,
  LuminanceAlpha,
  RGB,
  RGBA,
  Depth24,
  Depth24Stencil8,

  -- * Classes
  InternalFormat(..),
  Framebuffer(..),
  Texture(..),
  Attachable(..)

) where

import Graphics.GL.Low.Classes
import Graphics.GL.Low.VAO
import Graphics.GL.Low.BufferObject
import Graphics.GL.Low.Shader
import Graphics.GL.Low.VertexAttrib
import Graphics.GL.Low.Texture
import Graphics.GL.Low.Framebuffer
import Graphics.GL.Low.Blending
import Graphics.GL.Low.Color
import Graphics.GL.Low.Depth
import Graphics.GL.Low.Stencil
import Graphics.GL.Low.Render
import Graphics.GL.Low.ImageFormat
import Graphics.GL.Low.Cube
import Graphics.GL.Low.Error
