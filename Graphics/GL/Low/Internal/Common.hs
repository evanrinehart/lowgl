module Graphics.GL.Low.Internal.Common where

import Data.Monoid
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Control.Exception
import Control.Monad (liftM)
import Control.Monad.IO.Class

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import qualified Data.Text.Encoding as TE
import Graphics.GL

import Graphics.GL.Low.Internal.Types
import Graphics.GL.Low.Classes


throwM :: (MonadIO m, Exception e) => e -> m a
throwM = liftIO . throwIO

getGLstring :: (MonadIO m, Integral a) => a -> (Ptr GLsizei -> Ptr GLchar -> IO ()) -> m Text
getGLstring msz f = do
    str <- liftIO . alloca $ \lenb -> BI.createAndTrim (fromIntegral msz) $ \strb -> do
        f lenb (castPtr strb)
        liftM fromIntegral $ peek lenb
    return $ TE.decodeUtf8 str

withGLstring :: Text -> (Ptr GLchar -> IO a) -> IO a
withGLstring s act = B.useAsCString (TE.encodeUtf8 s) $  act . castPtr


cubeSideCodes :: Cube GLenum
cubeSideCodes = Cube
  { cubeLeft   = GL_TEXTURE_CUBE_MAP_NEGATIVE_X
  , cubeRight  = GL_TEXTURE_CUBE_MAP_POSITIVE_X
  , cubeTop    = GL_TEXTURE_CUBE_MAP_POSITIVE_Y
  , cubeBottom = GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
  , cubeFront  = GL_TEXTURE_CUBE_MAP_POSITIVE_Z
  , cubeBack   = GL_TEXTURE_CUBE_MAP_NEGATIVE_Z }

