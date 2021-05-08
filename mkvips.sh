#!/usr/bin/env sh
#
# Generate hvips code by introspection of libvips.
# Execute in the root directory of the hvips git repository.
#
echo "Generating Vips/Introspection/Operations.hs..." && \
stack exec hvips-gen -- -f hvips-gen/headers/types.hs types > src/Vips/Introspection/Operations.hs && \
echo "Generating Vips/Arguments.hs..." && \
stack exec hvips-gen -- -f hvips-gen/headers/args.hs args > src/Vips/Arguments.hs && \
echo "Generating Vips/Results.hs..." && \
stack exec hvips-gen -- -f hvips-gen/headers/results.hs results > src/Vips/Results.hs && \
echo "Generating Vips/Operations.hs..." && \
stack exec hvips-gen -- -f hvips-gen/headers/ops.hs ops > src/Vips/Operations.hs
