{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.GL.Low.Error where

import Control.Exception
import Data.Typeable

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

-- | Check for a GL Error.
getGLError :: IO (Maybe GLError)
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
