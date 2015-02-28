module Graphics.GL.Low.VertexAttrib (
-- | To feed vertices into the vertex shader, the layout of a vertex must be
-- specified in the current VAO for the current shader program. Make a list of
-- LayoutElements and use 'setVertexLayout' on it as seen below.
--
-- @
-- setVertexLayout
--   [ Attrib "position"  3 GLFloat   -- first 12 bytes maps to: in vec3 position;
--   , Attrib "shininess" 1 GLFloat   -- next 4 bytes maps to:   in float shininess;
--   , Attrib "texcoord"  2 GLFloat   -- next 8 bytes maps to:   in vec2 texcoord;
--   , Unused 2                       -- next 2 bytes ignored
--   , Attrib "seed"      1 GLShort ] -- next 2 bytes read as 16-bit signed int
--                                    --   and mapped to: in float seed;
-- @
--
-- In this example four mappings from the current VBO to the variables
-- in the current Program will be established in the current VAO.

  setVertexLayout,
  VertexLayout(..),
  GLFloatType(..), Signedness(..), GLScalarType(..), GLVectorSize(..), GLAttribType(..)
) where


import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad (forM_)
import Control.Monad.IO.Class

import Graphics.GL
import Graphics.GL.Low.Internal.Types
import Graphics.GL.Low.Classes

-- | The name of a vertex input to a program combined with the
-- component format and number of components for that attribute in the
-- vertex data. Alternatively the size of an unused section of the data
-- in bytes.
data VertexLayout =
  Attrib String Int GLAttribType | -- ^ Name, component count and component format of a vertex attribute.
  Unused Int -- ^ Size in bytes of an unused section of the vertex data.
    deriving Show

-- | This configures the currently bound VAO. It calls glVertexAttribPointer
-- and glEnableVertexAttribArray.
setVertexLayout :: (MonadIO m) => [VertexLayout] -> m ()
setVertexLayout layout = liftIO $ do
  p <- alloca (\ptr -> glGetIntegerv GL_CURRENT_PROGRAM ptr >> peek ptr)
  if p == 0
    then return ()
    else do
      let layout' = elaborateLayout 0 layout
      let total = totalLayout layout
      forM_ layout' $ \(name, size, offset, fmt) -> do
        attrib <- withCString name $ \ptr -> glGetAttribLocation (fromIntegral p) (castPtr ptr)
        let stride = total
        glVertexAttribPointer
          (fromIntegral attrib)
          (fromIntegral size)
          (toGL fmt)
          GL_FALSE
          (fromIntegral stride)
          (castPtr (nullPtr `plusPtr` offset))
        glEnableVertexAttribArray (fromIntegral attrib)

elaborateLayout :: Int -> [VertexLayout] -> [(String, Int, Int, GLAttribType)]
elaborateLayout here layout = case layout of
  [] -> []
  (Unused n):xs -> elaborateLayout (here+n) xs
  (Attrib name n ty):xs ->
    let size = n * sizeOfType ty in
    (name, n, here, ty) : elaborateLayout (here+size) xs

totalLayout :: [VertexLayout] -> Int
totalLayout layout = sum (map arraySize layout) where
  arraySize (Unused n) = n
  arraySize (Attrib _ n ty) = n * sizeOfType ty

sizeOfType :: GLAttribType -> Int
sizeOfType c = case c of
  GLScalarAttrib (GLFloat Single) -> 4
  GLScalarAttrib (GLFloat Double) -> 8
  GLScalarAttrib (GLInteger _)    -> 4
  GLVectorAttrib s Two            -> 2 * sizeOfType (GLScalarAttrib s)
  GLVectorAttrib s Three          -> 3 * sizeOfType (GLScalarAttrib s)
  GLVectorAttrib s Four           -> 4 * sizeOfType (GLScalarAttrib s)
  GLMatrixAttrib s n Two          -> 2 * sizeOfType (GLVectorAttrib (GLFloat s) n)
  GLMatrixAttrib s n Three        -> 3 * sizeOfType (GLVectorAttrib (GLFloat s) n)
  GLMatrixAttrib s n Four         -> 4 * sizeOfType (GLVectorAttrib (GLFloat s) n)


