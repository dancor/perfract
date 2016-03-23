// Sutherland-Hodgman polygon clipping

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gmp.h"
 
typedef double f_t;

#include "png.c"
#include "mp_vec.c"
#include "mp_poly.c"
#include "mp_poly_render.c"
#include "rat_rot.c"
#include "aug_m.c"

typedef struct { mp_poly p; } rec_fig_t, *rec_fig;

int main()
{
    aug_m_t a;
    bitmap_t b;
    mp_poly p = mp_poly_new();
    mp_vec_t v1;
    mp_vec_t v2;
    mp_vec_t v3;
    mp_vec_t v4;
    mp_vec_t trans_v;
    mpq_t n;
    rat_rot_t r1, r2;

    bitmap_init(&b, 512, 512);
    bitmap_blackout(&b);
    mpq_init(n);
    mpq_set_si(n, 400, 1);
    mp_vec_init_set(&trans_v, 256, 1, 256, 1);
    
    make_rat_rot(&r1, 1, 8);
    make_rat_rot(&r2, 7, 100);
    a_init_id(&a);
    a_rotate_by(&a, &r1);
    a_scale_by(&a, n);
    a_translate_by(&a, &trans_v);

    mp_vec_init_set(&v1, -1, 10, -1, 10);
    mp_vec_init_set(&v2,  1, 10, -1, 10);
    mp_vec_init_set(&v3,  1, 10,  1, 10);
    mp_vec_init_set(&v4, -1, 10,  1, 10);
    mp_poly_append(p, &v1);
    mp_poly_append(p, &v2);
    mp_poly_append(p, &v3);
    mp_poly_append(p, &v4);
    a_p_apply(p, &a);
    //debug_print_poly("p-before-render_poly", p);

    mp_poly_render(&b, p);
    save_png_to_file(&b, "out.png");

    a_clear(&a);
    bitmap_free(&b);
    mp_poly_free(p);
    mp_vec_clear(&v1);
    mp_vec_clear(&v2);
    mp_vec_clear(&v3);
    mp_vec_clear(&v4);
    mp_vec_clear(&trans_v);
    mpq_clear(n);
}
