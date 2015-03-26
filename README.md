Perfract: Perfectly-rasterized fractals
=

So far we have:

- Perfect rasterization of nonoverlapping (but possibly touching)
convex polygons.

Notes:

1. Our fractals are convex polygons with interated
rational affine transformations.
2. Note that rational affine rotations are necessarily
irrational-degree rotations (except e.g. right-angle rotations)
but that any rational-degree rotation can be approximated arbitrarily well.
3. Note that intersecting arbitrary-precision-rational-point polygons is
probably always slow so we should be smart and obviate it as much as possible.

Next goals:

- Set up a performance test, then look at different optimizations
  (multithreading, using Float or such for the canvas..).
- Separate rendering thread.
- Determining expansion cutoff to give perfect fractal rasterization.
- Smart clipping of offscreen polygons.
- Smart way to walk through fractal spine.
- Handle overlapping polygon pieces?
- Make everything a lot faster.
- Color changing and how that is specified.
- More general computable graphics primitive?
  A countably infinite set of polygons that gets smaller sufficiently
  quickly and sufficiently locally..
