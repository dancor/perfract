// Sutherland-Hodgman polygon clipping

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
 
#include "gmp.h"

int vec_num = 1;

typedef struct { int num; mpq_t x, y; } vec_t, *vec;

void vec_init(vec v)
{
    mpq_init(v->x);
    mpq_init(v->y);

    v->num = vec_num;
    vec_num++;
    printf("vec_init %d\n", v->num);
}

void vec_init_set(vec v, int xn, int xd, int yn, int yd) {
    vec_init(v);
    mpq_set_si(v->x, xn, xd);
    mpq_set_si(v->y, yn, yd);
}

void vec_clear(vec v)
{
    mpq_clear(v->x);
    mpq_clear(v->y);

    printf("vec_clear %d\n", v->num);
}

void cross(mpq_t res, const vec a, const vec b)
{
    mpq_t tmp1, tmp2;

    mpq_init(tmp1);
    mpq_init(tmp2);

    mpq_mul(tmp1, a->x, b->y);
    mpq_mul(tmp2, a->y, b->x);
    mpq_sub(res, tmp1, tmp2);

    mpq_clear(tmp1);
    mpq_clear(tmp2);
}
 
void vsub(vec res, vec a, vec b)
{
    mpq_sub(res->x, a->x, b->x);
    mpq_sub(res->y, a->y, b->y);
}
 
// Does vec c lie on the left side of directed edge a->b?
// 1 if left, -1 if right, 0 if colinear
int c_left_of_ab(vec a, vec b, vec c)
{
    vec_t tmp1, tmp2;
    mpq_t x;
    int res;

    vec_init(&tmp1);
    vec_init(&tmp2);
    mpq_init(x);

    vsub(&tmp1, b, a);
    vsub(&tmp2, c, b);
    cross(x, &tmp1, &tmp2);
    res = mpq_sgn(x);

    vec_clear(&tmp1);
    vec_clear(&tmp2);
    mpq_clear(x);

    return res;
}

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
 
void poly_append(poly p, vec v)
{
    if (p->len >= p->alloc) {
        p->alloc *= 2;
        if (!p->alloc) p->alloc = 4;
        p->v = (vec)realloc(p->v, sizeof(vec_t) * p->alloc);
    }
    p->v[p->len++] = *v;
}

// Required after this function:
// - when the retval is 1, p has a vec appended which will need a vec_clear()
int isect_lines_poly_append(poly p, poly p_new, vec x0, vec x1, vec y0, vec y1)
{
    vec_t dx, dy, tmp;
    mpq_t dyx;
    int ret;

    vec_init(&dx);
    vec_init(&dy);
    mpq_init(dyx);

    // x0 + a dx = y0 + b dy ->
    // x0 X dx = y0 X dx + b dy X dx ->
    // b = (x0 - y0) X dx / (dy X dx)
    vsub(&dx, x1, x0);
    vsub(&dy, y1, y0);
    cross(dyx, &dy, &dx);
    // TODO: mpq_equal must be faster?
    if (!mpq_sgn(dyx)) {
        ret = 0;
        goto line_sect_ret;
    }

    vec_t d;
    mpq_t dyx2;

    vec_init(&d);
    mpq_init(dyx2);

    vsub(&d, x0, y0);
    cross(dyx2, &d, &dx);

    vec_clear(&d);

    mpq_div(dyx, dyx2, dyx);
    if (mpq_cmp_si(dyx, 0, 1) <= 0 || mpq_cmp_si(dyx, 1, 1) >= 0) {
        ret = 0;
        goto line_sect_ret;
    }
 
    mpq_set(dyx2, dyx);

    mpq_mul(dyx, dyx, dy.x);

    vec_t res;
    printf("isect init:\n");
    vec_init(&res);
    mpq_add(res.x, y0->x, dyx);

    mpq_mul(dyx2, dyx2, dy.y);
    mpq_add(res.y, y0->y, dyx2);
    
    poly_append(p, &res);
    poly_append(p_new, &res);

    ret = 1;

line_sect_ret:
    
    vec_clear(&dx);
    vec_clear(&dy);
    mpq_clear(dyx);
    mpq_clear(dyx2);

    return ret;
}
 
// Required before a call to this function:
// - res->len = res_new->len = 0
// Required after this function:
// - the vec's in res_new will each need a vec_clear()
void poly_edge_clip(poly res, poly res_new, poly sub, vec clip0, vec clip1)
{
    int i, side0, side1;
    vec tmp;
    vec v0 = sub->v + sub->len - 1;
    vec v1;

    side0 = c_left_of_ab(clip0, clip1, v0);
    if (side0 != -1) {
        poly_append(res, v0);
    }

    int app = 0;
    for (i = 0; ; i++) {
        v1 = sub->v + i;
        side1 = c_left_of_ab(clip0, clip1, v1);
        if (side0 + side1 == 0 && side0) {
            // last point and current straddle the edge
            isect_lines_poly_append(res, res_new, clip0, clip1, v0, v1);
            /*
            app += 1;
            printf("clip0 ");
            mpq_out_str(stdout, 10, clip0->x);
            printf(",");
            mpq_out_str(stdout, 10, clip0->y);
            printf(" clip1 ");
            mpq_out_str(stdout, 10, clip1->x);
            printf(",");
            mpq_out_str(stdout, 10, clip1->y);
            printf(" v0 ");
            mpq_out_str(stdout, 10, v0->x);
            printf(",");
            mpq_out_str(stdout, 10, v0->y);
            printf(" v1 ");
            mpq_out_str(stdout, 10, v1->x);
            printf(",");
            mpq_out_str(stdout, 10, v1->y);
            puts("");
            printf("doing append %d %d: ", app, tmp.num);
            mpq_out_str(stdout, 10, tmp.x);
            printf(" ");
            mpq_out_str(stdout, 10, tmp.y);
            puts("");
            */
        }
        if (i == sub->len - 1) {
            break;
        }
        if (side1 != -1) {
            poly_append(res, v1);
        }
        v0 = v1;
        side0 = side1;
    }
}
 
// Required after this function:
// - the vec's in the returned poly will each need a vec_clear()
// - the returned poly will need a poly_free()
poly poly_clip(poly sub, poly clip)
{
    poly p1 = poly_new();
    poly p2 = poly_new();
    poly p2_new = poly_new();
    poly tmp;
 
    poly_edge_clip(p2, p2_new, sub, clip->v + clip->len - 1, clip->v);
    for (int i = 0; i < clip->len - 1; i++) {
        tmp = p2; p2 = p1; p1 = tmp;

        for (int j = 0; j < p2_new->len; j++) {
            //vec_clear(&(p2_new->v[j]));
            printf("would have cleared: %d\n", p2_new->v[j].num);
            //break;
        }
        p2_new->len = 0;
        p2->len = 0;

        if (p1->len == 0) {
            break;
        }
        poly_edge_clip(p2, p2_new, p1, clip->v + i, clip->v + i + 1);
    }
 
    poly_free(p1);
    return p2;
}
 
int main()
{
    vec_t s1, s2, s3, s4, s5, s6, s7, s8, s9;
    vec_t c1, c2, c3, c4;
    poly clipper = poly_new();
    poly subject = poly_new();

    vec_init_set(&s1,  50, 1, 150, 1);
    vec_init_set(&s2, 200, 1,  50, 1);
    vec_init_set(&s3, 350, 1, 150, 1);
    vec_init_set(&s4, 350, 1, 300, 1);
    vec_init_set(&s5, 250, 1, 300, 1);
    vec_init_set(&s6, 200, 1, 250, 1);
    vec_init_set(&s7, 150, 1, 350, 1);
    vec_init_set(&s8, 100, 1, 250, 1);
    vec_init_set(&s9, 100, 1, 200, 1);
    vec_init_set(&c1, 100, 1, 100, 1);
    vec_init_set(&c2, 300, 1, 100, 1);
    vec_init_set(&c3, 300, 1, 300, 1);
    vec_init_set(&c4, 100, 1, 300, 1);
    poly_append(subject, &s1);
    poly_append(subject, &s2);
    poly_append(subject, &s3);
    poly_append(subject, &s4);
    poly_append(subject, &s5);
    poly_append(subject, &s6);
    poly_append(subject, &s7);
    poly_append(subject, &s8);
    poly_append(subject, &s9);
    poly_append(clipper, &c1);
    poly_append(clipper, &c2);
    poly_append(clipper, &c3);
    poly_append(clipper, &c4);
 
    poly res = poly_clip(subject, clipper);

    for (int i = 0; i < res->len; i++) {
        //printf("%g %g\n", res->v[i].x, res->v[i].y);
        //printf("%d: ", res->v[i].num);
        mpq_out_str(stdout, 10, res->v[i].x);
        printf(" ");
        mpq_out_str(stdout, 10, res->v[i].y);
        puts("");
    }

    poly_free(res);
    vec_clear(&s1);
    vec_clear(&s2);
    vec_clear(&s3);
    vec_clear(&s4);
    vec_clear(&s5);
    vec_clear(&s6);
    vec_clear(&s7);
    vec_clear(&s8);
    vec_clear(&s9);
    vec_clear(&c1);
    vec_clear(&c2);
    vec_clear(&c3);
    vec_clear(&c4);
}
