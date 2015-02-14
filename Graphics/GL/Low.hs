module Graphics.GL.Low (

  -- * In a Nutshell
  --
  -- ** Overview
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
  --
  -- (include link to example programs)

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
  -- | The role of the second half of a program, the fragment shader, is to
  -- compute the color and depth of pixels covered by rasterized primitives
  -- (points, lines, and triangles) in the process of rendering. The role of
  -- the /first/ half of the program (vertex program) is to arrange the vertices
  -- of those primitives somewhere in clip space. Where these vertices and
  -- their attributes come from in the first place is determined by the VAO
  -- bound to the vertex array binding target. The program may also make use
  -- of uniform variables and texture units assigned by client code before
  -- rendering (but in a process separate from configuring the VAO). At most
  -- one Program can be "in use" at a time.

  -- ** VAO
  -- | The VAO is essential. At least one VAO must be created and bound to the
  -- vertex array binding target before rendering, before configuring a
  -- program's vertex attributes. Here is why: the VAO stores the association
  -- between input variables in the program and a VBO from which to pipe input
  -- from. It also stores the format of the VBO data, which is otherwise just
  -- a big blob. Finally, the VAO stores the state of the element array binding
  -- target used for indexed rendering.
  --
  -- After installing a program with 'useProgram' and binding a source VBO
  -- to the array buffer binding target ('bindVBO') then the bound VAO can be
  -- updated ('setVertexAttributeLayout') with new vertex attribute information.
  -- After this, the VBO can be rebound to configure a different set of inputs
  -- with a different source. Many VAOs can be created and swapped out to pipe
  -- vertex data in different ways to different programs (or the same program).
  --
  -- When a VAO is bound it restores the state of the element array binding
  -- target. For this reason you can think of that binding target as simply
  -- being a function of the VAO itself rather than a separate global state.

  -- ** Uniforms and Samplers (Textures)
  -- | Programs may have uniform variables and "sampler uniforms" as input.
  -- Uniforms are accessible from the vertex or fragment shader part of the
  -- program but their values are fixed during the course of a rendering command.
  -- They can be set and reset with the setUniform family (ex. 'setUniform1f'),
  -- which updates a program object with new uniform values. Among other
  -- things, updating the uniforms each frame is the main way to animate a
  -- scene.
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
  -- | It is possible (and important to many techniques) to utilize an
  -- off-screen render target. To do this create an FBO ('newFBO'), bind it to
  -- the framebuffer binding target ('bindFramebuffer') and attach a color
  -- /image/ object (texture or renderbuffer object). If necessary a depth
  -- image or combination depth-stencil image can be attached as well. If no
  -- color image is attached then the FBO is incomplete and rendering will be
  -- an error.  After rendering to an FBO any textures that were attached can
  -- be used for a second pass by assigning them to a texture unit. Watch out
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
  -- of the 16-bit color formats rather than 32.)

  -- ** Depth Testing, Stencil Testing, Scissor Testing, Facet Culling
  -- | The depth buffer and stencil buffers, if present in the current
  -- framebuffer, can be used to avoid rendering to points of the screen by
  -- testing against the value stored at those points. For example if commanded
  -- to show a triangle in a region of the framebuffer with a depth greater
  -- than current depth buffer values, then the triangle may not be rendered to
  -- the color buffer or anywhere else (depending on settings). There are many
  -- global settings to switch on and off these tests and the ability to
  -- modify the buffers involved. The stencil test in particular is highly
  -- configurable. The scissor test is the simplest: when activated nothing
  -- outside the scissor box (in screen space) will be rendered. The only
  -- other configuration is to set that scissor box ('setScissorBox').
  -- Polygons facing toward or away from the viewer can be dropped (or culled)
  -- from rendering with 'enableCulling'.

  -- ** Coordinate Systems
  -- | - Screen space is simply the 2D coordinate system of your window.
  -- The viewport transformation (see 'setViewport') determines where in the
  -- window the mapping of the NDS cube (see below) will appear.
  -- - NDS, normalized device coordinates, or sometimes viewport space is a
  -- cube 2x2x2 centered at the origin the inside of which is your final scene,
  -- before it is mapped to the screen via the viewport setting (see
  -- 'setViewport'). If an orthographic projection was used to put the scene in
  -- clip space then clip space and NDS are the same.
  -- - Clip space is the destination of vertices transformed by the
  -- vertex program. Objects here are mapped to NDS using the perspective
  -- division technique to account for the case that the vertex shader used
  -- a perspective matrix.
  -- - Model space is the name for positions of raw vertices as present in
  -- the VBOs. The vertex program will want to somehow move these vertexes
  -- into clip space, representing generally the position and direction the
  -- user is viewing the scene from.

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
  IndexFormat(..),

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
  setVertexAttributeLayout,
  VertexAttributeLayout(..),
  LayoutElement(..),
  ComponentFormat(..),


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
  -- | See also "Graphics.GL.Low.Render"

  -- ** Primitives
  -- | Draw primitives to the framebuffer currently bound to the framebuffer
  -- binding target. Each primitive drawing command takes the number of vertices
  -- in the VBOs to render. The vertices are traversed in order.
  drawPoints,
  drawLines,
  drawLineStrip,
  drawLineLoop,
  drawTriangles,
  drawTriangleStrip,
  drawTriangleFan,

  -- ** Primitives by Index
  -- | Draw primitives as above, but use the order of vertices defined in
  -- the ElementArray currently bound to the element array buffer binding
  -- target.
  drawIndexedPoints,
  drawIndexedLines,
  drawIndexedLineStrip,
  drawIndexedLineLoop,
  drawIndexedTriangles,
  drawIndexedTriangleStrip,
  drawIndexedTriangleFan,

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

  -- ** Scissor Test
  enableScissorTest,
  disableScissorTest,

  -- ** Facet Culling
  Culling(..),
  enableCulling,
  disableCulling,

  -- ** Blending
  -- | See also "Graphics.GL.Low.Blending".

  enableBlending,
  disableBlending,
  basicBlending,
  Blending(..),
  BlendFactor(..),
  BlendEquation(..),

  -- ** Viewport
  Viewport(..),
  setViewport,

  -- * Framebuffers
  -- | See also "Graphics.GL.Low.Framebuffer"
  DefaultFramebuffer,
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
