// Sutherland-Hodgman polygon clipping

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gmp.h"
 
#include "png.h"

//
// vec
//

typedef struct { float x, y; } vec_t, *vec;

inline float cross(vec a, vec b)
{
    return a->x * b->y - a->y * b->x;
}
 
inline vec vsub(vec a, vec b, vec res)
{
    res->x = a->x - b->x;
    res->y = a->y - b->y;
}
 
// Does point c lie on the left side of directed edge a->b?
// 1 if left, -1 if right, 0 if on the line
int c_left_of_ab(vec a, vec b, vec c)
{
    vec_t tmp1, tmp2;
    float x;
    vsub(b, a, &tmp1);
    vsub(c, b, &tmp2);
    x = cross(&tmp1, &tmp2);
    return x < 0 ? -1 : x > 0;
}
 
//
// poly
//

typedef struct { int len, alloc; vec v; } poly_t, *poly;
 
poly poly_new()
{
    return (poly)calloc(1, sizeof(poly_t));
}
 
void poly_free(poly p)
{
    free(p->v);
    free(p);
}
 
void poly_append(poly p, const vec v)
{
    if (p->len >= p->alloc) {
        if (p->alloc) {
          p->alloc *= 2;
        } else {
          p->alloc = 4;
        }
        p->v = (vec)realloc(p->v, sizeof(vec_t) * p->alloc);
    }
    p->v[p->len++] = *v;
}

void isect_lines_poly_append(poly p, vec x0, vec x1, vec y0, vec y1)
{
    vec_t dx, dy, d;
    vsub(x1, x0, &dx);
    vsub(y1, y0, &dy);
    vsub(x0, y0, &d);
    /* x0 + a dx = y0 + b dy ->
       x0 X dx = y0 X dx + b dy X dx ->
       b = (x0 - y0) X dx / (dy X dx) */
    float dyx = cross(&dy, &dx);
    if (!dyx) {
        return;
    }
    dyx = cross(&d, &dx) / dyx;
    if (dyx <= 0 || dyx >= 1) {
        return;
    }
 
    d.x = y0->x + dyx * dy.x;
    d.y = y0->y + dyx * dy.y;
    poly_append(p, &d);
}

void poly_edge_clip(poly sub, vec x0, vec x1, poly res)
{
    int i, side0, side1;
    vec v0 = sub->v + sub->len - 1, v1;
    res->len = 0;
 
    side0 = c_left_of_ab(x0, x1, v0);
    if (side0 != -1) poly_append(res, v0);
 
    for (i = 0; i < sub->len; i++) {
        v1 = sub->v + i;
        side1 = c_left_of_ab(x0, x1, v1);
        if (side0 + side1 == 0 && side0) {
            // last point and current straddle the edge
            isect_lines_poly_append(res, x0, x1, v0, v1);
        }
        if (i == sub->len - 1) break;
        if (side1 != -1) poly_append(res, v1);
        v0 = v1;
        side0 = side1;
    }
}
 
poly poly_clip(poly sub, poly clip)
{
    int i;
    poly p1 = poly_new(), p2 = poly_new(), tmp;
 
    poly_edge_clip(sub, clip->v + clip->len - 1, clip->v, p2);
    for (i = 0; i < clip->len - 1; i++) {
        tmp = p2; p2 = p1; p1 = tmp;
        if(p1->len == 0) {
            p2->len = 0;
            break;
        }
        poly_edge_clip(p1, clip->v + i, clip->v + i + 1, p2);
    }
 
    poly_free(p1);
    return p2;
}

float poly_area(poly p) {
    int i_last = p->len - 1;
    float sum = p->v[i_last].x * p->v[0].y - p->v[0].x * p->v[i_last].y;
    for (int i = 0; i < i_last;) {
        int i2 = i + 1;
        sum += p->v[i].x * p->v[i2].y - p->v[i2].x * p->v[i].y;
        i = i2;
    }
    return 0.5f * sum;
}

/*
#define BSIZE 512
#define TOP (-0.5f)
#define LEFT (0.5f)
#define PIXELWIDTH (1/512.0f)
*/

void render_poly(bitmap_t *b, poly p) {
    vec_t v;
    poly_t pixel;
    float xf;
    float xf2;
    float yf;
    float yf2;
    pixel_t *my_pixel = b->pixels;

    poly_append(&pixel, &v);
    poly_append(&pixel, &v);
    poly_append(&pixel, &v);
    poly_append(&pixel, &v);
    for (int y = 0, yf = 0; y < b->h; y++) {
        yf2 = yf + 1.0f;
        for (int x = 0, xf = 0; x < b->w; x++) {
            xf2 = xf + 1.0f;
            pixel.v[0].x = xf;
            pixel.v[0].y = yf;
            pixel.v[1].x = xf2;
            pixel.v[1].y = yf;
            pixel.v[2].x = xf2;
            pixel.v[2].y = yf2;
            pixel.v[3].x = xf;
            pixel.v[3].y = yf2;
            poly res = poly_clip(p, &pixel);
            if (res->len) {
                my_pixel->r =
                    (int)(poly_area(poly_clip(p, &pixel)) * 255.0f + 0.5);
            }
            xf = xf2;
            my_pixel++;
        }
        yf = yf2;
    }
}

//
// rat_rot
//

typedef struct { mpq_t sin, cos; } rat_rot_t, *rat_rot;

void make_rat_rot(rat_rot r, int cw_rot_n, int cw_rot_d) {
    assert(cw_rot_d > 0);
    assert(-2 * cw_rot_n < cw_rot_d); // rot > -1/2
    assert(2 * cw_rot_n <= cw_rot_d); // rot <= 1/2

    double theta = -2 * M_PI * (double)cw_rot_n / (double)cw_rot_d;
    double y_over_x = tan(theta);
    double n_over_m_approx = y_over_x + sqrt(1 + y_over_x * y_over_x);
    int m = 100;
    int n = round (n_over_m_approx * m);
    int n2 = n * n;
    int m2 = m * m;
    int denom = n2 + m2;
    mpq_init(r->sin);
    mpq_init(r->cos);
    mpq_set_si(r->sin, n2 - m2, denom);
    mpq_set_si(r->cos, 2 * n * m, denom);
}

void rat_rot_clear(rat_rot r)
{
    mpq_clear(r->sin);
    mpq_clear(r->cos);
}

//
// mp_vec
//

typedef struct { mpq_t x, y; } mp_vec_t, *mp_vec;

void mp_vec_init(mp_vec v)
{
    mpq_init(v->x);
    mpq_init(v->y);
}

void mp_vec_init_set(mp_vec v, int xn, int xd, int yn, int yd)
{
    mp_vec_init(v);
    mpq_set_si(v->x, xn, xd);
    mpq_set_si(v->y, yn, yd);
}

void mp_vec_clear(mp_vec v)
{
    mpq_clear(v->x);
    mpq_clear(v->y);
}

void mp_vec_to_vec(vec v, const mp_vec mv) {
    v->x = mpq_get_d(mv->x);
    v->y = mpq_get_d(mv->y);
}

//
// mp_poly
//

typedef struct { int len, alloc; mp_vec v; } mp_poly_t, *mp_poly;

mp_poly mp_poly_new()
{
    return (mp_poly)calloc(1, sizeof(poly_t));
}

void mp_poly_free(mp_poly p)
{
    free(p->v);
    free(p);
}

void mp_poly_append(mp_poly p, const mp_vec v)
{
    if (p->len >= p->alloc) {
        if (p->alloc) {
          p->alloc *= 2;
        } else {
          p->alloc = 4;
        }
        p->v = (mp_vec)realloc(p->v, sizeof(mp_vec_t) * p->alloc);
    }
    p->v[p->len++] = *v;
}

void mp_poly_to_poly(poly p, const mp_poly mp) {
    vec_t v;
    for (int i = 0; i < mp->len; i++) {
        mp_vec_to_vec(&v, &mp->v[i]);
        poly_append(p, &v);
    }
}

//
// aug_m
//

typedef struct { mpq_t x, y, a11, a12, a21, a22; } aug_m_t, *aug_m;

void a_init(aug_m a) {
    mpq_init(a->x);
    mpq_init(a->y);
    mpq_init(a->a11);
    mpq_init(a->a12);
    mpq_init(a->a21);
    mpq_init(a->a22);
}

void a_clear(aug_m a) {
    mpq_clear(a->x);
    mpq_clear(a->y);
    mpq_clear(a->a11);
    mpq_clear(a->a12);
    mpq_clear(a->a21);
    mpq_clear(a->a22);
}

void a_init_id(aug_m a) {
    a_init(a);
    mpq_set_si(a->a11, 1, 1);
    mpq_set_si(a->a22, 1, 1);
}

void a_translate_by(aug_m a, const mp_vec v) {
    mpq_add(a->x, a->x, v->x);
    mpq_add(a->y, a->y, v->y);
}

void a_scale_by(aug_m a, const mpq_t n) {
    mpq_mul(a->x, a->x, n);
    mpq_mul(a->y, a->y, n);
    mpq_mul(a->a11, a->a11, n);
    mpq_mul(a->a12, a->a12, n);
    mpq_mul(a->a21, a->a21, n);
    mpq_mul(a->a22, a->a22, n);
}

void a_rotate_by(aug_m a, const rat_rot r) {
    mpq_t tmp_s;
    mpq_t tmp_c;
    
    mpq_init(tmp_s);
    mpq_init(tmp_c);

    mpq_mul(tmp_s , a->a11, r->sin);
    mpq_mul(tmp_c , a->a11, r->cos);
    mpq_mul(a->a11, a->a21, r->sin);
    mpq_sub(a->a11, tmp_c , a->a11);
    mpq_mul(a->a21, a->a21, r->cos);
    mpq_add(a->a21, tmp_s , a->a21);

    mpq_mul(tmp_s , a->a12, r->sin);
    mpq_mul(tmp_c , a->a12, r->cos);
    mpq_mul(a->a12, a->a22, r->sin);
    mpq_sub(a->a12, tmp_c , a->a12);
    mpq_mul(a->a22, a->a22, r->cos);
    mpq_add(a->a22, tmp_s , a->a22);

    mpq_mul(tmp_s, a->x , r->sin);
    mpq_mul(tmp_c, a->x , r->cos);
    mpq_mul(a->x , a->y , r->sin);
    mpq_sub(a->x , tmp_c, a->x  );
    mpq_mul(a->y , a->y , r->cos);
    mpq_add(a->y , tmp_s, a->y  );

    mpq_clear(tmp_s);
    mpq_clear(tmp_c);
}

void a_apply(mp_vec v, const aug_m a) {
    mpq_t tmp;

    mpq_init(tmp);

    // x = a11 * x + a12 * y + ax
    mpq_mul(tmp, a->a12, v->y);
    mpq_mul(v->x, a->a11, v->x);
    mpq_add(v->x, v->x, tmp);
    mpq_add(v->x, v->x, a->x);

    // y = a21 * x + a22 * y + ay
    mpq_mul(tmp, a->a21, v->x);
    mpq_mul(v->y, a->a22, v->y);
    mpq_add(v->y, v->y, tmp);
    mpq_add(v->y, v->y, a->y);

    mpq_clear(tmp);
}

int main()
{
    aug_m_t a;
    bitmap_t b;
    mp_poly mp = mp_poly_new();
    poly p = poly_new();
    mp_vec_t mv1, mv2, mv3, mv4;

    bitmap_init(&b, 512, 512);
    bitmap_blackout(&b);
    
    a_init_id(&a);

    mp_vec_init_set(&mv1, 100, 1, 100, 1);
    mp_vec_init_set(&mv2, 300, 1, 100, 1);
    mp_vec_init_set(&mv3, 300, 1, 300, 1);
    mp_vec_init_set(&mv4, 100, 1, 300, 1);
    mp_poly_append(mp, &mv1);
    mp_poly_append(mp, &mv2);
    mp_poly_append(mp, &mv3);
    mp_poly_append(mp, &mv4);
    mp_poly_to_poly(p, mp);

    render_poly(&b, p);
    save_png_to_file(&b, "out.png");

    a_clear(&a);
    bitmap_free(&b);
    mp_poly_free(mp);
    mp_vec_clear(&mv1);
    mp_vec_clear(&mv2);
    mp_vec_clear(&mv3);
    mp_vec_clear(&mv4);
}
