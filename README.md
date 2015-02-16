# lowgl

Use modern but basic OpenGL from Haskell with this wrapper around the amazing
gl package. The gl package exposes the entire OpenGL API and even
auto-configures extensions available on your system. However it is a direct
translation of the C API, which means all the low level calls require a medium
amount of FFI negotation. Worse than that is the amount of identifiers that
GL_ARE_FORMATTED_LIKE_THIS. Also, as is rightly pointed out, the OpenGL C API
has very little in the way of type safety. Besides safety, what's interesting
to me is the amount of guidance a minimal amount of types can provide to such a
complex interface. The square pegs go in the square holes! Even so the
incredibly stateful semantics of OpenGL require a good bit of english to
accurately describe. 

# Install

```
cabal install lowgl
```

