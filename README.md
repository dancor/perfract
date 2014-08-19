Perfract: Perfectly-rasterized fractals
=

So far we have:

- Perfect rasterization of nonoverlapping (but possibly touching)
convex polygons.

Notes:

1. Our fractals are convex polygons with interated
rational affine transformations.
2. Note that rational affine rotations are necessarily
irrational-degree rotations (excepting e.g. right-angle rotations)
but that any rational-degree rotation can be approximated arbitrarily well.
3. Note that intersecting arbitrary-precision-rational-point polygons is
probably always slow so we should be smart and obviate it as much as possible.

Next goals:

1. Determining expansion cutoff to give perfect fractal rasterization.
2. Smart clipping of offscreen polygons.
3. Smart way to walk through fractal spine.
4. Handle overlapping polygon pieces?
5. Make everything a lot faster.
