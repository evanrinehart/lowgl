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
  DataType(..)
) where


import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad (forM_)

import Graphics.GL
import Graphics.GL.Low.Classes

-- | The name of a vertex input to a program combined with the
-- component format and number of components for that attribute in the
-- vertex data. Alternatively the size of an unused section of the data
-- in bytes.
data VertexLayout =
  Attrib String Int DataType | -- ^ Name, component count and component format of a vertex attribute.
  Unused Int -- ^ Size in bytes of an unused section of the vertex data.
    deriving Show

-- | The size and interpretation of a vertex attribute component.
data DataType =
  GLFloat         | -- ^ 4-byte float
  GLByte          | -- ^ signed byte
  GLUnsignedByte  | -- ^ unsigned byte
  GLShort         | -- ^ 2-byte signed integer
  GLUnsignedShort | -- ^ 2-byte unsigned integer
  GLInt           | -- ^ 4-byte signed integer
  GLUnsignedInt     -- ^ 4-byte unsigned integer
    deriving (Eq, Show)

-- | This configures the currently bound VAO. It calls glVertexAttribPointer
-- and glEnableVertexAttribArray.
setVertexLayout :: [VertexLayout] -> IO ()
setVertexLayout layout = do
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


instance ToGL DataType where
  toGL GLFloat         = GL_FLOAT
  toGL GLByte          = GL_BYTE
  toGL GLUnsignedByte  = GL_UNSIGNED_BYTE
  toGL GLShort         = GL_SHORT
  toGL GLUnsignedShort = GL_UNSIGNED_SHORT
  toGL GLInt           = GL_INT
  toGL GLUnsignedInt   = GL_UNSIGNED_INT

instance ToGL Bool where
  toGL True = GL_TRUE
  toGL False = GL_FALSE

elaborateLayout :: Int -> [VertexLayout] -> [(String, Int, Int, DataType)]
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

sizeOfType :: DataType -> Int
sizeOfType c = case c of
  GLFloat         -> 4
  GLByte          -> 1
  GLUnsignedByte  -> 1
  GLShort         -> 2
  GLUnsignedShort -> 2
  GLInt           -> 4
  GLUnsignedInt   -> 4
