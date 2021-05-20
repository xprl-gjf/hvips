{-# LANGUAGE DataKinds,
             MultiParamTypeClasses,
             FlexibleContexts #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Vips.Arguments where

import qualified GI.Vips as GV
import           Vips.Introspection.Operations

--
-- Vips named arguments:
--

-- Note:
-- "img" is used as a synonym for "in",
-- "imgs" is used as a synonym for "in" for an array of images
-- since `in` is a Haskell reserved word

--
-- The following code has been automatically generated using hvips-gen,
-- from libvips 8.10.6-Sat Apr 24 19:02:40 UTC 2021
--


a' :: (HasArgument a "a" b) => b -> a -> a
a' = set (Set :: Argument "a")

access' :: (HasArgument a "access" b) => b -> a -> a
access' = set (Set :: Argument "access")

across' :: (HasArgument a "across" b) => b -> a -> a
across' = set (Set :: Argument "across")

align' :: (HasArgument a "align" b) => b -> a -> a
align' = set (Set :: Argument "align")

alphaBand' :: (HasArgument a "alpha-band" b) => b -> a -> a
alphaBand' = set (Set :: Argument "alpha-band")

alphaQ' :: (HasArgument a "alpha-q" b) => b -> a -> a
alphaQ' = set (Set :: Argument "alpha-q")

amplitudeCutoff' :: (HasArgument a "amplitude-cutoff" b) => b -> a -> a
amplitudeCutoff' = set (Set :: Argument "amplitude-cutoff")

angle' :: (HasArgument a "angle" b) => b -> a -> a
angle' = set (Set :: Argument "angle")

ascii' :: (HasArgument a "ascii" b) => b -> a -> a
ascii' = set (Set :: Argument "ascii")

autorotate' :: (HasArgument a "autorotate" b) => b -> a -> a
autorotate' = set (Set :: Argument "autorotate")

b' :: (HasArgument a "b" b) => b -> a -> a
b' = set (Set :: Argument "b")

background' :: (HasArgument a "background" b) => b -> a -> a
background' = set (Set :: Argument "background")

band' :: (HasArgument a "band" b) => b -> a -> a
band' = set (Set :: Argument "band")

bandno' :: (HasArgument a "bandno" b) => b -> a -> a
bandno' = set (Set :: Argument "bandno")

bands' :: (HasArgument a "bands" b) => b -> a -> a
bands' = set (Set :: Argument "bands")

base' :: (HasArgument a "base" b) => b -> a -> a
base' = set (Set :: Argument "base")

basename' :: (HasArgument a "basename" b) => b -> a -> a
basename' = set (Set :: Argument "basename")

bigtiff' :: (HasArgument a "bigtiff" b) => b -> a -> a
bigtiff' = set (Set :: Argument "bigtiff")

bins' :: (HasArgument a "bins" b) => b -> a -> a
bins' = set (Set :: Argument "bins")

bitdepth' :: (HasArgument a "bitdepth" b) => b -> a -> a
bitdepth' = set (Set :: Argument "bitdepth")

blend' :: (HasArgument a "blend" b) => b -> a -> a
blend' = set (Set :: Argument "blend")

boolean' :: (HasArgument a "boolean" b) => b -> a -> a
boolean' = set (Set :: Argument "boolean")

buffer' :: (HasArgument a "buffer" b) => b -> a -> a
buffer' = set (Set :: Argument "buffer")

c' :: (HasArgument a "c" b) => b -> a -> a
c' = set (Set :: Argument "c")

cases' :: (HasArgument a "cases" b) => b -> a -> a
cases' = set (Set :: Argument "cases")

cellSize' :: (HasArgument a "cell-size" b) => b -> a -> a
cellSize' = set (Set :: Argument "cell-size")

centre' :: (HasArgument a "centre" b) => b -> a -> a
centre' = set (Set :: Argument "centre")

cluster' :: (HasArgument a "cluster" b) => b -> a -> a
cluster' = set (Set :: Argument "cluster")

cmdFormat' :: (HasArgument a "cmd-format" b) => b -> a -> a
cmdFormat' = set (Set :: Argument "cmd-format")

cmplx' :: (HasArgument a "cmplx" b) => b -> a -> a
cmplx' = set (Set :: Argument "cmplx")

coding' :: (HasArgument a "coding" b) => b -> a -> a
coding' = set (Set :: Argument "coding")

coeff' :: (HasArgument a "coeff" b) => b -> a -> a
coeff' = set (Set :: Argument "coeff")

combine' :: (HasArgument a "combine" b) => b -> a -> a
combine' = set (Set :: Argument "combine")

compositingSpace' :: (HasArgument a "compositing-space" b) => b -> a -> a
compositingSpace' = set (Set :: Argument "compositing-space")

compression' :: (HasArgument a "compression" b) => b -> a -> a
compression' = set (Set :: Argument "compression")

cond' :: (HasArgument a "cond" b) => b -> a -> a
cond' = set (Set :: Argument "cond")

container' :: (HasArgument a "container" b) => b -> a -> a
container' = set (Set :: Argument "container")

crop' :: (HasArgument a "crop" b) => b -> a -> a
crop' = set (Set :: Argument "crop")

csize' :: (HasArgument a "csize" b) => b -> a -> a
csize' = set (Set :: Argument "csize")

cx' :: (HasArgument a "cx" b) => b -> a -> a
cx' = set (Set :: Argument "cx")

cy' :: (HasArgument a "cy" b) => b -> a -> a
cy' = set (Set :: Argument "cy")

density' :: (HasArgument a "density" b) => b -> a -> a
density' = set (Set :: Argument "density")

depth' :: (HasArgument a "depth" b) => b -> a -> a
depth' = set (Set :: Argument "depth")

direction' :: (HasArgument a "direction" b) => b -> a -> a
direction' = set (Set :: Argument "direction")

dither' :: (HasArgument a "dither" b) => b -> a -> a
dither' = set (Set :: Argument "dither")

down' :: (HasArgument a "down" b) => b -> a -> a
down' = set (Set :: Argument "down")

dpi' :: (HasArgument a "dpi" b) => b -> a -> a
dpi' = set (Set :: Argument "dpi")

dsize' :: (HasArgument a "dsize" b) => b -> a -> a
dsize' = set (Set :: Argument "dsize")

dx' :: (HasArgument a "dx" b) => b -> a -> a
dx' = set (Set :: Argument "dx")

dy' :: (HasArgument a "dy" b) => b -> a -> a
dy' = set (Set :: Argument "dy")

embedded' :: (HasArgument a "embedded" b) => b -> a -> a
embedded' = set (Set :: Argument "embedded")

equal' :: (HasArgument a "equal" b) => b -> a -> a
equal' = set (Set :: Argument "equal")

esize' :: (HasArgument a "esize" b) => b -> a -> a
esize' = set (Set :: Argument "esize")

exp' :: (HasArgument a "exp" b) => b -> a -> a
exp' = set (Set :: Argument "exp")

expand' :: (HasArgument a "expand" b) => b -> a -> a
expand' = set (Set :: Argument "expand")

exponent' :: (HasArgument a "exponent" b) => b -> a -> a
exponent' = set (Set :: Argument "exponent")

exportProfile' :: (HasArgument a "export-profile" b) => b -> a -> a
exportProfile' = set (Set :: Argument "export-profile")

extend' :: (HasArgument a "extend" b) => b -> a -> a
extend' = set (Set :: Argument "extend")

factor' :: (HasArgument a "factor" b) => b -> a -> a
factor' = set (Set :: Argument "factor")

fail' :: (HasArgument a "fail" b) => b -> a -> a
fail' = set (Set :: Argument "fail")

fd' :: (HasArgument a "fd" b) => b -> a -> a
fd' = set (Set :: Argument "fd")

filename' :: (HasArgument a "filename" b) => b -> a -> a
filename' = set (Set :: Argument "filename")

fill' :: (HasArgument a "fill" b) => b -> a -> a
fill' = set (Set :: Argument "fill")

filter' :: (HasArgument a "filter" b) => b -> a -> a
filter' = set (Set :: Argument "filter")

font' :: (HasArgument a "font" b) => b -> a -> a
font' = set (Set :: Argument "font")

fontfile' :: (HasArgument a "fontfile" b) => b -> a -> a
fontfile' = set (Set :: Argument "fontfile")

format' :: (HasArgument a "format" b) => b -> a -> a
format' = set (Set :: Argument "format")

fractalDimension' :: (HasArgument a "fractal-dimension" b) => b -> a -> a
fractalDimension' = set (Set :: Argument "fractal-dimension")

frequencyCutoff' :: (HasArgument a "frequency-cutoff" b) => b -> a -> a
frequencyCutoff' = set (Set :: Argument "frequency-cutoff")

frequencyCutoffX' :: (HasArgument a "frequency-cutoff-x" b) => b -> a -> a
frequencyCutoffX' = set (Set :: Argument "frequency-cutoff-x")

frequencyCutoffY' :: (HasArgument a "frequency-cutoff-y" b) => b -> a -> a
frequencyCutoffY' = set (Set :: Argument "frequency-cutoff-y")

gamma' :: (HasArgument a "gamma" b) => b -> a -> a
gamma' = set (Set :: Argument "gamma")

get' :: (HasArgument a "get" b) => b -> a -> a
get' = set (Set :: Argument "get")

h' :: (HasArgument a "h" b) => b -> a -> a
h' = set (Set :: Argument "h")

h_ :: (HasArgument a "H" b) => b -> a -> a
h_ = set (Set :: Argument "H")

halign' :: (HasArgument a "halign" b) => b -> a -> a
halign' = set (Set :: Argument "halign")

harea' :: (HasArgument a "harea" b) => b -> a -> a
harea' = set (Set :: Argument "harea")

height' :: (HasArgument a "height" b) => b -> a -> a
height' = set (Set :: Argument "height")

hfreq' :: (HasArgument a "hfreq" b) => b -> a -> a
hfreq' = set (Set :: Argument "hfreq")

hshrink' :: (HasArgument a "hshrink" b) => b -> a -> a
hshrink' = set (Set :: Argument "hshrink")

hspacing' :: (HasArgument a "hspacing" b) => b -> a -> a
hspacing' = set (Set :: Argument "hspacing")

hwindow' :: (HasArgument a "hwindow" b) => b -> a -> a
hwindow' = set (Set :: Argument "hwindow")

id' :: (HasArgument a "id" b) => b -> a -> a
id' = set (Set :: Argument "id")

idx' :: (HasArgument a "idx" b) => b -> a -> a
idx' = set (Set :: Argument "idx")

idy' :: (HasArgument a "idy" b) => b -> a -> a
idy' = set (Set :: Argument "idy")

image' :: (HasArgument a "image" b) => b -> a -> a
image' = set (Set :: Argument "image")

img :: (HasArgument a "in" GV.Image) => GV.Image -> a -> a
img = set (Set :: Argument "in")

imgs :: (HasArgument a "in" GV.ArrayImage) => GV.ArrayImage -> a -> a
imgs = set (Set :: Argument "in")

importProfile' :: (HasArgument a "import-profile" b) => b -> a -> a
importProfile' = set (Set :: Argument "import-profile")

in1' :: (HasArgument a "in1" b) => b -> a -> a
in1' = set (Set :: Argument "in1")

in2' :: (HasArgument a "in2" b) => b -> a -> a
in2' = set (Set :: Argument "in2")

inFormat' :: (HasArgument a "in-format" b) => b -> a -> a
inFormat' = set (Set :: Argument "in-format")

inMax' :: (HasArgument a "in-max" b) => b -> a -> a
inMax' = set (Set :: Argument "in-max")

index' :: (HasArgument a "index" b) => b -> a -> a
index' = set (Set :: Argument "index")

ink' :: (HasArgument a "ink" b) => b -> a -> a
ink' = set (Set :: Argument "ink")

input' :: (HasArgument a "input" b) => b -> a -> a
input' = set (Set :: Argument "input")

inputProfile' :: (HasArgument a "input-profile" b) => b -> a -> a
inputProfile' = set (Set :: Argument "input-profile")

intOutput' :: (HasArgument a "int-output" b) => b -> a -> a
intOutput' = set (Set :: Argument "int-output")

intent' :: (HasArgument a "intent" b) => b -> a -> a
intent' = set (Set :: Argument "intent")

interesting' :: (HasArgument a "interesting" b) => b -> a -> a
interesting' = set (Set :: Argument "interesting")

interlace' :: (HasArgument a "interlace" b) => b -> a -> a
interlace' = set (Set :: Argument "interlace")

interpolate' :: (HasArgument a "interpolate" b) => b -> a -> a
interpolate' = set (Set :: Argument "interpolate")

interpretation' :: (HasArgument a "interpretation" b) => b -> a -> a
interpretation' = set (Set :: Argument "interpretation")

justify' :: (HasArgument a "justify" b) => b -> a -> a
justify' = set (Set :: Argument "justify")

kernel' :: (HasArgument a "kernel" b) => b -> a -> a
kernel' = set (Set :: Argument "kernel")

kmax' :: (HasArgument a "kmax" b) => b -> a -> a
kmax' = set (Set :: Argument "kmax")

kmin' :: (HasArgument a "kmin" b) => b -> a -> a
kmin' = set (Set :: Argument "kmin")

layers' :: (HasArgument a "layers" b) => b -> a -> a
layers' = set (Set :: Argument "layers")

layout' :: (HasArgument a "layout" b) => b -> a -> a
layout' = set (Set :: Argument "layout")

lb_ :: (HasArgument a "Lb" b) => b -> a -> a
lb_ = set (Set :: Argument "Lb")

left' :: (HasArgument a "left" b) => b -> a -> a
left' = set (Set :: Argument "left")

level' :: (HasArgument a "level" b) => b -> a -> a
level' = set (Set :: Argument "level")

linear' :: (HasArgument a "linear" b) => b -> a -> a
linear' = set (Set :: Argument "linear")

lines' :: (HasArgument a "lines" b) => b -> a -> a
lines' = set (Set :: Argument "lines")

log' :: (HasArgument a "log" b) => b -> a -> a
log' = set (Set :: Argument "log")

lossless' :: (HasArgument a "lossless" b) => b -> a -> a
lossless' = set (Set :: Argument "lossless")

lut' :: (HasArgument a "lut" b) => b -> a -> a
lut' = set (Set :: Argument "lut")

lw_ :: (HasArgument a "Lw" b) => b -> a -> a
lw_ = set (Set :: Argument "Lw")

m' :: (HasArgument a "m" b) => b -> a -> a
m' = set (Set :: Argument "m")

m0' :: (HasArgument a "m0" b) => b -> a -> a
m0' = set (Set :: Argument "m0")

m1' :: (HasArgument a "m1" b) => b -> a -> a
m1' = set (Set :: Argument "m1")

m2' :: (HasArgument a "m2" b) => b -> a -> a
m2' = set (Set :: Argument "m2")

m_ :: (HasArgument a "M" b) => b -> a -> a
m_ = set (Set :: Argument "M")

main' :: (HasArgument a "main" b) => b -> a -> a
main' = set (Set :: Argument "main")

mask' :: (HasArgument a "mask" b) => b -> a -> a
mask' = set (Set :: Argument "mask")

math' :: (HasArgument a "math" b) => b -> a -> a
math' = set (Set :: Argument "math")

math2' :: (HasArgument a "math2" b) => b -> a -> a
math2' = set (Set :: Argument "math2")

matrix' :: (HasArgument a "matrix" b) => b -> a -> a
matrix' = set (Set :: Argument "matrix")

maxAlpha' :: (HasArgument a "max-alpha" b) => b -> a -> a
maxAlpha' = set (Set :: Argument "max-alpha")

maxRadius' :: (HasArgument a "max-radius" b) => b -> a -> a
maxRadius' = set (Set :: Argument "max-radius")

maxSlope' :: (HasArgument a "max-slope" b) => b -> a -> a
maxSlope' = set (Set :: Argument "max-slope")

maxTiles' :: (HasArgument a "max-tiles" b) => b -> a -> a
maxTiles' = set (Set :: Argument "max-tiles")

mblend' :: (HasArgument a "mblend" b) => b -> a -> a
mblend' = set (Set :: Argument "mblend")

mean' :: (HasArgument a "mean" b) => b -> a -> a
mean' = set (Set :: Argument "mean")

memory' :: (HasArgument a "memory" b) => b -> a -> a
memory' = set (Set :: Argument "memory")

minAmpl' :: (HasArgument a "min-ampl" b) => b -> a -> a
minAmpl' = set (Set :: Argument "min-ampl")

minRadius' :: (HasArgument a "min-radius" b) => b -> a -> a
minRadius' = set (Set :: Argument "min-radius")

minSize' :: (HasArgument a "min-size" b) => b -> a -> a
minSize' = set (Set :: Argument "min-size")

miniswhite' :: (HasArgument a "miniswhite" b) => b -> a -> a
miniswhite' = set (Set :: Argument "miniswhite")

mode' :: (HasArgument a "mode" b) => b -> a -> a
mode' = set (Set :: Argument "mode")

morph' :: (HasArgument a "morph" b) => b -> a -> a
morph' = set (Set :: Argument "morph")

n' :: (HasArgument a "n" b) => b -> a -> a
n' = set (Set :: Argument "n")

name' :: (HasArgument a "name" b) => b -> a -> a
name' = set (Set :: Argument "name")

nearLossless' :: (HasArgument a "near-lossless" b) => b -> a -> a
nearLossless' = set (Set :: Argument "near-lossless")

noRotate' :: (HasArgument a "no-rotate" b) => b -> a -> a
noRotate' = set (Set :: Argument "no-rotate")

noStrip' :: (HasArgument a "no-strip" b) => b -> a -> a
noStrip' = set (Set :: Argument "no-strip")

nodc' :: (HasArgument a "nodc" b) => b -> a -> a
nodc' = set (Set :: Argument "nodc")

oarea' :: (HasArgument a "oarea" b) => b -> a -> a
oarea' = set (Set :: Argument "oarea")

odx' :: (HasArgument a "odx" b) => b -> a -> a
odx' = set (Set :: Argument "odx")

ody' :: (HasArgument a "ody" b) => b -> a -> a
ody' = set (Set :: Argument "ody")

offset' :: (HasArgument a "offset" b) => b -> a -> a
offset' = set (Set :: Argument "offset")

optical' :: (HasArgument a "optical" b) => b -> a -> a
optical' = set (Set :: Argument "optical")

optimizeCoding' :: (HasArgument a "optimize-coding" b) => b -> a -> a
optimizeCoding' = set (Set :: Argument "optimize-coding")

optimizeGifFrames' :: (HasArgument a "optimize-gif-frames" b) => b -> a -> a
optimizeGifFrames' = set (Set :: Argument "optimize-gif-frames")

optimizeGifTransparency' :: (HasArgument a "optimize-gif-transparency" b) => b -> a -> a
optimizeGifTransparency' = set (Set :: Argument "optimize-gif-transparency")

optimizeScans' :: (HasArgument a "optimize-scans" b) => b -> a -> a
optimizeScans' = set (Set :: Argument "optimize-scans")

optionString' :: (HasArgument a "option-string" b) => b -> a -> a
optionString' = set (Set :: Argument "option-string")

order' :: (HasArgument a "order" b) => b -> a -> a
order' = set (Set :: Argument "order")

outFormat' :: (HasArgument a "out-format" b) => b -> a -> a
outFormat' = set (Set :: Argument "out-format")

outMax' :: (HasArgument a "out-max" b) => b -> a -> a
outMax' = set (Set :: Argument "out-max")

outputProfile' :: (HasArgument a "output-profile" b) => b -> a -> a
outputProfile' = set (Set :: Argument "output-profile")

overlap' :: (HasArgument a "overlap" b) => b -> a -> a
overlap' = set (Set :: Argument "overlap")

overlay' :: (HasArgument a "overlay" b) => b -> a -> a
overlay' = set (Set :: Argument "overlay")

overshootDeringing' :: (HasArgument a "overshoot-deringing" b) => b -> a -> a
overshootDeringing' = set (Set :: Argument "overshoot-deringing")

page' :: (HasArgument a "page" b) => b -> a -> a
page' = set (Set :: Argument "page")

pageHeight' :: (HasArgument a "page-height" b) => b -> a -> a
pageHeight' = set (Set :: Argument "page-height")

palette' :: (HasArgument a "palette" b) => b -> a -> a
palette' = set (Set :: Argument "palette")

pcs' :: (HasArgument a "pcs" b) => b -> a -> a
pcs' = set (Set :: Argument "pcs")

percent' :: (HasArgument a "percent" b) => b -> a -> a
percent' = set (Set :: Argument "percent")

persistent' :: (HasArgument a "persistent" b) => b -> a -> a
persistent' = set (Set :: Argument "persistent")

ph_ :: (HasArgument a "Ph" b) => b -> a -> a
ph_ = set (Set :: Argument "Ph")

pm_ :: (HasArgument a "Pm" b) => b -> a -> a
pm_ = set (Set :: Argument "Pm")

point' :: (HasArgument a "point" b) => b -> a -> a
point' = set (Set :: Argument "point")

precision' :: (HasArgument a "precision" b) => b -> a -> a
precision' = set (Set :: Argument "precision")

predictor' :: (HasArgument a "predictor" b) => b -> a -> a
predictor' = set (Set :: Argument "predictor")

premultiplied' :: (HasArgument a "premultiplied" b) => b -> a -> a
premultiplied' = set (Set :: Argument "premultiplied")

preset' :: (HasArgument a "preset" b) => b -> a -> a
preset' = set (Set :: Argument "preset")

profile' :: (HasArgument a "profile" b) => b -> a -> a
profile' = set (Set :: Argument "profile")

properties' :: (HasArgument a "properties" b) => b -> a -> a
properties' = set (Set :: Argument "properties")

ps_ :: (HasArgument a "Ps" b) => b -> a -> a
ps_ = set (Set :: Argument "Ps")

pyramid' :: (HasArgument a "pyramid" b) => b -> a -> a
pyramid' = set (Set :: Argument "pyramid")

q_ :: (HasArgument a "Q" b) => b -> a -> a
q_ = set (Set :: Argument "Q")

quality' :: (HasArgument a "quality" b) => b -> a -> a
quality' = set (Set :: Argument "quality")

quantTable' :: (HasArgument a "quant-table" b) => b -> a -> a
quantTable' = set (Set :: Argument "quant-table")

radius' :: (HasArgument a "radius" b) => b -> a -> a
radius' = set (Set :: Argument "radius")

real' :: (HasArgument a "real" b) => b -> a -> a
real' = set (Set :: Argument "real")

reductionEffort' :: (HasArgument a "reduction-effort" b) => b -> a -> a
reductionEffort' = set (Set :: Argument "reduction-effort")

ref' :: (HasArgument a "ref" b) => b -> a -> a
ref' = set (Set :: Argument "ref")

regionShrink' :: (HasArgument a "region-shrink" b) => b -> a -> a
regionShrink' = set (Set :: Argument "region-shrink")

reject' :: (HasArgument a "reject" b) => b -> a -> a
reject' = set (Set :: Argument "reject")

relational' :: (HasArgument a "relational" b) => b -> a -> a
relational' = set (Set :: Argument "relational")

resunit' :: (HasArgument a "resunit" b) => b -> a -> a
resunit' = set (Set :: Argument "resunit")

right' :: (HasArgument a "right" b) => b -> a -> a
right' = set (Set :: Argument "right")

ringwidth' :: (HasArgument a "ringwidth" b) => b -> a -> a
ringwidth' = set (Set :: Argument "ringwidth")

round' :: (HasArgument a "round" b) => b -> a -> a
round' = set (Set :: Argument "round")

s0' :: (HasArgument a "s0" b) => b -> a -> a
s0' = set (Set :: Argument "s0")

s_ :: (HasArgument a "S" b) => b -> a -> a
s_ = set (Set :: Argument "S")

scale' :: (HasArgument a "scale" b) => b -> a -> a
scale' = set (Set :: Argument "scale")

search' :: (HasArgument a "search" b) => b -> a -> a
search' = set (Set :: Argument "search")

sec' :: (HasArgument a "sec" b) => b -> a -> a
sec' = set (Set :: Argument "sec")

separable' :: (HasArgument a "separable" b) => b -> a -> a
separable' = set (Set :: Argument "separable")

separator' :: (HasArgument a "separator" b) => b -> a -> a
separator' = set (Set :: Argument "separator")

shift' :: (HasArgument a "shift" b) => b -> a -> a
shift' = set (Set :: Argument "shift")

shim' :: (HasArgument a "shim" b) => b -> a -> a
shim' = set (Set :: Argument "shim")

shrink' :: (HasArgument a "shrink" b) => b -> a -> a
shrink' = set (Set :: Argument "shrink")

sigma' :: (HasArgument a "sigma" b) => b -> a -> a
sigma' = set (Set :: Argument "sigma")

size' :: (HasArgument a "size" b) => b -> a -> a
size' = set (Set :: Argument "size")

skip' :: (HasArgument a "skip" b) => b -> a -> a
skip' = set (Set :: Argument "skip")

skipBlanks' :: (HasArgument a "skip-blanks" b) => b -> a -> a
skipBlanks' = set (Set :: Argument "skip-blanks")

smartSubsample' :: (HasArgument a "smart-subsample" b) => b -> a -> a
smartSubsample' = set (Set :: Argument "smart-subsample")

source' :: (HasArgument a "source" b) => b -> a -> a
source' = set (Set :: Argument "source")

sourceSpace' :: (HasArgument a "source-space" b) => b -> a -> a
sourceSpace' = set (Set :: Argument "source-space")

space' :: (HasArgument a "space" b) => b -> a -> a
space' = set (Set :: Argument "space")

spacing' :: (HasArgument a "spacing" b) => b -> a -> a
spacing' = set (Set :: Argument "spacing")

speed' :: (HasArgument a "speed" b) => b -> a -> a
speed' = set (Set :: Argument "speed")

strip' :: (HasArgument a "strip" b) => b -> a -> a
strip' = set (Set :: Argument "strip")

sub' :: (HasArgument a "sub" b) => b -> a -> a
sub' = set (Set :: Argument "sub")

subifd' :: (HasArgument a "subifd" b) => b -> a -> a
subifd' = set (Set :: Argument "subifd")

subsampleMode' :: (HasArgument a "subsample-mode" b) => b -> a -> a
subsampleMode' = set (Set :: Argument "subsample-mode")

suffix' :: (HasArgument a "suffix" b) => b -> a -> a
suffix' = set (Set :: Argument "suffix")

target' :: (HasArgument a "target" b) => b -> a -> a
target' = set (Set :: Argument "target")

temp' :: (HasArgument a "temp" b) => b -> a -> a
temp' = set (Set :: Argument "temp")

test' :: (HasArgument a "test" b) => b -> a -> a
test' = set (Set :: Argument "test")

tests' :: (HasArgument a "tests" b) => b -> a -> a
tests' = set (Set :: Argument "tests")

text' :: (HasArgument a "text" b) => b -> a -> a
text' = set (Set :: Argument "text")

threaded' :: (HasArgument a "threaded" b) => b -> a -> a
threaded' = set (Set :: Argument "threaded")

threshold' :: (HasArgument a "threshold" b) => b -> a -> a
threshold' = set (Set :: Argument "threshold")

thumbnail' :: (HasArgument a "thumbnail" b) => b -> a -> a
thumbnail' = set (Set :: Argument "thumbnail")

tile' :: (HasArgument a "tile" b) => b -> a -> a
tile' = set (Set :: Argument "tile")

tileHeight' :: (HasArgument a "tile-height" b) => b -> a -> a
tileHeight' = set (Set :: Argument "tile-height")

tileSize' :: (HasArgument a "tile-size" b) => b -> a -> a
tileSize' = set (Set :: Argument "tile-size")

tileWidth' :: (HasArgument a "tile-width" b) => b -> a -> a
tileWidth' = set (Set :: Argument "tile-width")

times' :: (HasArgument a "times" b) => b -> a -> a
times' = set (Set :: Argument "times")

top' :: (HasArgument a "top" b) => b -> a -> a
top' = set (Set :: Argument "top")

trellisQuant' :: (HasArgument a "trellis-quant" b) => b -> a -> a
trellisQuant' = set (Set :: Argument "trellis-quant")

uchar' :: (HasArgument a "uchar" b) => b -> a -> a
uchar' = set (Set :: Argument "uchar")

unlimited' :: (HasArgument a "unlimited" b) => b -> a -> a
unlimited' = set (Set :: Argument "unlimited")

ushort' :: (HasArgument a "ushort" b) => b -> a -> a
ushort' = set (Set :: Argument "ushort")

v' :: (HasArgument a "v" b) => b -> a -> a
v' = set (Set :: Argument "v")

valign' :: (HasArgument a "valign" b) => b -> a -> a
valign' = set (Set :: Argument "valign")

vfreq' :: (HasArgument a "vfreq" b) => b -> a -> a
vfreq' = set (Set :: Argument "vfreq")

vscale' :: (HasArgument a "vscale" b) => b -> a -> a
vscale' = set (Set :: Argument "vscale")

vshrink' :: (HasArgument a "vshrink" b) => b -> a -> a
vshrink' = set (Set :: Argument "vshrink")

vspacing' :: (HasArgument a "vspacing" b) => b -> a -> a
vspacing' = set (Set :: Argument "vspacing")

whitespace' :: (HasArgument a "whitespace" b) => b -> a -> a
whitespace' = set (Set :: Argument "whitespace")

width' :: (HasArgument a "width" b) => b -> a -> a
width' = set (Set :: Argument "width")

x' :: (HasArgument a "x" b) => b -> a -> a
x' = set (Set :: Argument "x")

x1' :: (HasArgument a "x1" b) => b -> a -> a
x1' = set (Set :: Argument "x1")

x2' :: (HasArgument a "x2" b) => b -> a -> a
x2' = set (Set :: Argument "x2")

xfac' :: (HasArgument a "xfac" b) => b -> a -> a
xfac' = set (Set :: Argument "xfac")

xoffset' :: (HasArgument a "xoffset" b) => b -> a -> a
xoffset' = set (Set :: Argument "xoffset")

xr1' :: (HasArgument a "xr1" b) => b -> a -> a
xr1' = set (Set :: Argument "xr1")

xr2' :: (HasArgument a "xr2" b) => b -> a -> a
xr2' = set (Set :: Argument "xr2")

xref' :: (HasArgument a "xref" b) => b -> a -> a
xref' = set (Set :: Argument "xref")

xres' :: (HasArgument a "xres" b) => b -> a -> a
xres' = set (Set :: Argument "xres")

xs1' :: (HasArgument a "xs1" b) => b -> a -> a
xs1' = set (Set :: Argument "xs1")

xs2' :: (HasArgument a "xs2" b) => b -> a -> a
xs2' = set (Set :: Argument "xs2")

xsec' :: (HasArgument a "xsec" b) => b -> a -> a
xsec' = set (Set :: Argument "xsec")

y' :: (HasArgument a "y" b) => b -> a -> a
y' = set (Set :: Argument "y")

y1' :: (HasArgument a "y1" b) => b -> a -> a
y1' = set (Set :: Argument "y1")

y2' :: (HasArgument a "y2" b) => b -> a -> a
y2' = set (Set :: Argument "y2")

y3' :: (HasArgument a "y3" b) => b -> a -> a
y3' = set (Set :: Argument "y3")

yfac' :: (HasArgument a "yfac" b) => b -> a -> a
yfac' = set (Set :: Argument "yfac")

yoffset' :: (HasArgument a "yoffset" b) => b -> a -> a
yoffset' = set (Set :: Argument "yoffset")

yr1' :: (HasArgument a "yr1" b) => b -> a -> a
yr1' = set (Set :: Argument "yr1")

yr2' :: (HasArgument a "yr2" b) => b -> a -> a
yr2' = set (Set :: Argument "yr2")

yref' :: (HasArgument a "yref" b) => b -> a -> a
yref' = set (Set :: Argument "yref")

yres' :: (HasArgument a "yres" b) => b -> a -> a
yres' = set (Set :: Argument "yres")

ys1' :: (HasArgument a "ys1" b) => b -> a -> a
ys1' = set (Set :: Argument "ys1")

ys2' :: (HasArgument a "ys2" b) => b -> a -> a
ys2' = set (Set :: Argument "ys2")

ysec' :: (HasArgument a "ysec" b) => b -> a -> a
ysec' = set (Set :: Argument "ysec")
