hvips : haskell bindings for libvips
====================================

Experimental API for the [libvips](https://libvips.github.io/libvips/) image processing library, using the low-level [gi-vips](https://hackage.haskell.org/package/gi-vips-8.0.1) GObject introspection bindings.

## Status

Proof-of-concept only, but functional. Includes code for all libvips v8.10.6 operations. Sample `hvips-exe` demonstrates applying an inverse transform and saving an image, with no GObject memory leaks.

Still TODO:
* Add optional parameters to foreign operations (loadImage, saveImage).
* Add remaining foreign operations (loadImageBuffer, saveImageBuffer, etc).
* Support custom 'VipsSource' and 'VipsTargets', with callbacks to read/write image data.
* Implement efficient support for _mutable_ operations such as the various `DrawXXX` operations. See, for example, [ruby-vips](https://libvips.github.io/libvips/2021/03/08/ruby-vips-mutate.html) and [netvips](https://github.com/kleisauke/net-vips/issues/119) references for this issue.

## Quick-start

Install `libvips`; this is an external dependency, not managed by stack or cabal. Recommended version is 8.10.6.

Build and run the sample app via stack.

Run:
```console
stack exec hvips-exe -- -c <infile> <outfile>`
```

Infile and outfile may be different types. The type of outfile is determined by the file extension, e.g `hvips.exe img.jpg img.png`. Supported image types depend on the build of libvips; see the [libvips README](https://github.com/libvips/libvips#optional-dependencies).

### Mapping libvips operations to hvips functions

The hvips bindings supports the libvips image processing operations as per the [libvips docs](https://libvips.github.io/libvips/API/current/func-list.html).

Each libvips operation is exposed as a corresponding hvips function, with the 'vips_' prefix removed, and snake case converted to camel case. Mandatory parameters are arguments to the hvips function. Optional keyword parameters can be applied using the corresponding `Vips.Arguments` function. The complete list of arguments (mandatory and optional) for each operation can be found in [Introspection/Operations.hs](./src/Vips/Introspection/Operations.hs). By comparison, the corresponding function in [Operations.hs](./src/Vips/Operations.hs) shows the mandatory arguments.

For example, the [vips_gaussblur](https://libvips.github.io/libvips/API/current/libvips-convolution.html#vips-gaussblur) operation takes two mandatory parameters: `in` (the source image) and `sigma`. This operation also takes up to two optional parameters: `precision` and `min_ampl`.

The corresponding hvips call is therefore:

```haskell
  let img = someImage :: GI.Vips.Image
  let sigma = 1.2 :: Double
  let minAmpl' = 0.025 :: Double
  let prec' = GI.Vips.PrecisionFloat
  -- invoke vips_gaussblur with mandatory arguments only
  out <- vips . gaussblur sigma $ img
  -- alternatively, apply optional keyword arguments
  out' <- vips . blur $ img
  where
    blur = gaussblur sigma <&> Arg.minAmpl minAmpl' . Arg.precision prec' 
```

## Testing

To run tests with vips memory leak checking, run:
```console
VIPS_LEAK=true stack test
```
## Hacking hvips

Much of the code is auto-generated by introspection over libvips, using the low-level gi-vips capabilities.

To re-generate hvips code (e.g. for a different version of libvips), install the desired version of libvips and then run:
```console
./mkvips.sh && stack build
```
