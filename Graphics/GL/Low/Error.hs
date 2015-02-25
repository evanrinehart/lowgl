{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}

-- | If you issue an invalid GL command or run out of memory, the GL
-- implementation may set an internal flag indicating that an error has
-- occurred. You can query this flag ('getGLError') to see if something went
-- wrong with a previous command. If you ignore errors, chances are nothing
-- will happen except perhaps some rendering weirdness. However checking this
-- periodically is a good idea, perhaps once per frame, because it might
-- indicate a bug in your code.
module Graphics.GL.Low.Error (
  GLError(..),
  getGLError,
  assertNoGLError,
) where

import Control.Exception
import Data.Typeable
import Control.Monad.IO.Class

import Graphics.GL

-- | Detectable errors.
data GLError =
  InvalidEnum | -- ^ Enum argument out of range.
  InvalidValue | -- ^ Integer argument out of range.
  InvalidOperation | -- ^ Operation illegal in current state.
  InvalidFramebufferOperation | -- ^ Framebuffer is not complete.
  OutOfMemory
    deriving Typeable

instance Exception GLError

instance Show GLError where
  show InvalidEnum = "INVALID_ENUM enum argument out of range"
  show InvalidValue = "INVALID_VALUE Numeric argument out of range"
  show InvalidOperation = "INVALID_OPERATION Illegal in current state"
  show InvalidFramebufferOperation = "INVALID_FRAMEBUFFER_OPERATION Framebuffer object is not complete"
  show OutOfMemory = "Not enough memory left to execute command"

-- | Check for a GL Error. This call has the semantics of a dequeue. If an
-- error is returned, then calling getGLError again may return more errors that
-- have "stacked up." When it returns Nothing then there are no more errors to
-- report. An error indicates that a bug in your code caused incorrect ussage
-- of the API or that the implementation has run out of memory.
--
-- It has been suggested that using this after every single GL command may
-- adversely affect performance (not to mention be very tedious). Since there
-- is no reasonable way to recover from a GL error, a good idea might be to
-- check this once per frame or even less often, and respond with a core dump.
getGLError :: (MonadIO m) => m (Maybe GLError)
getGLError = do
  n <- glGetError
  return $ case n of
    GL_NO_ERROR -> Nothing
    GL_INVALID_ENUM -> Just InvalidEnum
    GL_INVALID_VALUE -> Just InvalidValue
    GL_INVALID_OPERATION -> Just InvalidOperation
    GL_INVALID_FRAMEBUFFER_OPERATION -> Just InvalidFramebufferOperation
    GL_OUT_OF_MEMORY -> Just OutOfMemory
    _ -> error ("unknown GL error " ++ show n)

-- | Throws an exception if 'getGLError' returns non-Nothing.
assertNoGLError :: (MonadIO m) => m ()
assertNoGLError = do
  me <- getGLError
  case me of
    Nothing -> return ()
    Just e  -> liftIO $ throwIO e
