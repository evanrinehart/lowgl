-- | Vertex Attribute Array.
module Graphics.GL.Low.VertexAttrib where

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
data LayoutElement =
  Attrib String Int ComponentFormat | -- ^ Name, component count and component format of a vertex attribute.
  Unused Int -- ^ Size in bytes of an unused section of the vertex data.
    deriving Show

-- | The layout of interleaved vertex attribute data.
type VertexAttributeLayout = [LayoutElement]

-- | The size and interpretation of a vertex attribute component. Normalized
-- components will be mapped to floats in the range [0, 1].
data ComponentFormat =
  VFloat | -- ^ 4-byte float
  VByte | 
  VUByte | 
  VByteNormalized | 
  VUByteNormalized |
  VShort | -- ^ 2-byte signed integer
  VUShort | -- ^ 2-byte unsigned integer
  VShortNormalized |
  VUShortNormalized |
  VInt | -- ^ 4-byte signed integer
  VUInt | -- ^ 4-byte unsigned integer
  VIntNormalized |
  VUIntNormalized
    deriving (Eq, Show)

instance ToGL ComponentFormat where
  toGL VFloat = GL_FLOAT
  toGL VByte = GL_BYTE
  toGL VUByte = GL_UNSIGNED_BYTE
  toGL VByteNormalized = GL_BYTE
  toGL VUByteNormalized = GL_UNSIGNED_BYTE
  toGL VShort = GL_SHORT
  toGL VUShort = GL_UNSIGNED_SHORT
  toGL VShortNormalized = GL_SHORT
  toGL VUShortNormalized = GL_UNSIGNED_SHORT
  toGL VInt = GL_INT
  toGL VUInt = GL_UNSIGNED_INT
  toGL VIntNormalized = GL_INT
  toGL VUIntNormalized = GL_UNSIGNED_INT
  


elaborateLayout :: Int -> VertexAttributeLayout -> [(String, Int, Int, ComponentFormat)]
elaborateLayout here layout = case layout of
  [] -> []
  (Unused n):xs -> elaborateLayout (here+n) xs
  (Attrib name n fmt):xs ->
    let size = n * sizeOfVertexComponent fmt in
    (name, n, here, fmt) : elaborateLayout (here+size) xs

totalLayout :: VertexAttributeLayout -> Int
totalLayout layout = sum (map arraySize layout) where
  arraySize (Unused n) = n
  arraySize (Attrib _ n fmt) = n * sizeOfVertexComponent fmt

sizeOfVertexComponent :: ComponentFormat -> Int
sizeOfVertexComponent c = case c of
  VByte -> 1
  VUByte -> 1
  VByteNormalized -> 1
  VUByteNormalized -> 1
  VShort -> 2
  VUShort -> 2
  VShortNormalized -> 2
  VUShortNormalized -> 2
  VInt -> 4
  VUInt -> 4
  VIntNormalized -> 4
  VUIntNormalized -> 4
  VFloat -> 4

isNormalized :: ComponentFormat -> Bool
isNormalized c = case c of
  VByte -> False
  VUByte -> False
  VByteNormalized -> True
  VUByteNormalized -> True
  VShort -> False
  VUShort -> False
  VShortNormalized -> True
  VUShortNormalized -> True
  VInt -> False
  VUInt -> False
  VIntNormalized -> True
  VUIntNormalized -> True
  VFloat -> False


-- | This configures the currently bound VAO. It calls glVertexAttribPointer
-- and glEnableVertexAttribArray.
setVertexAttributeLayout :: VertexAttributeLayout -> IO ()
setVertexAttributeLayout layout = do
  p <- alloca (\ptr -> glGetIntegerv GL_CURRENT_PROGRAM ptr >> peek ptr)
  if p == 0
    then return ()
    else do
      let layout' = elaborateLayout 0 layout
      let total = totalLayout layout
      forM_ layout' $ \(name, size, offset, fmt) -> do
        attrib <- withCString name $ \ptr -> glGetAttribLocation (fromIntegral p) (castPtr ptr)
        let stride = total - size * sizeOfVertexComponent fmt
        let norm = isNormalized fmt
        glVertexAttribPointer
          (fromIntegral attrib)
          (fromIntegral size)
          (toGL fmt)
          (toGL norm)
          (fromIntegral stride)
          (castPtr (nullPtr `plusPtr` offset))
        glEnableVertexAttribArray (fromIntegral attrib)

instance ToGL Bool where
  toGL True = GL_TRUE
  toGL False = GL_FALSE
