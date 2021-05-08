hvips-gen
=========


``` shellsession
hvips-gen - generate hvips code by introspection of libvips

Usage: hvips-gen [-f|--header <file>] COMMAND

Available options:
  -f,--header <file>       Prefix the generated output with the content of
                           <file>
  -h,--help                Show this help text

Available commands:
  ops                      Generate content for Introspection/Operations.hs
  args                     Generate content for Arguments.hs
  results                  Generate content for Results.hs
```

All the generated code for hvips can be created using the following script:

``` shell
#!/usr/bin/env sh
#
# Generate hvips code by introspection of libvips.
# Execute in the root directory of the hvips git repository.
#
echo "Generating Vips/Introspection/Operations.hs..." && \
stack exec hvips-gen -- -f hvips-gen/headers/ops.hs ops > src/Vips/Introspection/Operations.hs && \
echo "Generating Vips/Arguments.hs..." && \
stack exec hvips-gen -- -f hvips-gen/headers/args.hs args > src/Vips/Arguments.hs && \
echo "Generating Vips/Results.hs..." && \
stack exec hvips-gen -- -f hvips-gen/headers/results.hs results > src/Vips/Results.hs
```

