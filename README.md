# lowgl

Use modern but basic OpenGL from Haskell with this wrapper around the amazing
gl package. The gl package exposes the entire OpenGL API and even
auto-configures extensions available on your system. However it is a direct
translation of the C API, which means all the low level calls require a medium
amount of FFI negotation. Worse than that is the amount of identifiers that
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

