perfract: perfract.c png.c png.h mp_vec.c mp_poly.c mp_poly_render.c rat_rot.c aug_m.c
	gcc -march=native -O3 perfract.c -lgmp -lpng -lm -o perfract

debug: perfract.c png.c png.h mp_vec.c mp_poly.c mp_poly_render.c rat_rot.c aug_m.c
	gcc -march=native -g perfract.c -lgmp -lpng -lm -o perfract

#poly_clip_float: poly_clip_float.o png.o
#	gcc -march=native -O3 poly_clip_float.o png.o -lpng -o poly_clip_float
#   
#poly_clip_gmp: poly_clip_gmp.o png.o
#	gcc -march=native -O3 poly_clip_gmp.o png.o -lgmp -lpng -lm -o poly_clip_gmp
#   
#perfract.o: png.h perfract.c
#	gcc -march=native -O3 -c perfract.c
#
#poly_clip_float.o: png.h poly_clip_float.c
#	gcc -march=native -O3 -c poly_clip_float.c
#
#poly_clip_gmp.o: png.h poly_clip_gmp.c
#	gcc -march=native -O3 -c poly_clip_gmp.c
#
#png.o: png.h png.c
#	gcc -march=native -O3 -c png.c
