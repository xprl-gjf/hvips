hvips : haskell bindings for libvips
====================================

Experimental API for the [libvips](https://libvips.github.io/libvips/) image processing library, using the low-level [gi-vips](https://hackage.haskell.org/package/gi-vips-8.0.1) GObject introspection bindings.

## Status

Proof-of-concept only. Demonstrates applying an inverse transform and saving an image, with no GObject memory leaks. The intent would be to tidy up this code and then auto-generate the API calls for the remainder of the VIPS image transform functions (probably in a similar manner to the [C++ bindings](https://github.com/libvips/libvips/blob/master/doc/binding.md#compiled-language-which-can-call-c). But I may never get around to it.

## Quick-start

Build via stack.

Run:
```console
stack exec hvips-exe -- -c <infile> <outfile>`
```

Infile and outfile may be different types. The type of outfile is determined by the file extension, e.g `hvips.exe img.jpg img.png`. Supported image types depend on the build of libvips; see the [libvips README](https://github.com/libvips/libvips#optional-dependencies).


