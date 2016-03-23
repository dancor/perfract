typedef struct { int len, alloc; vec v; } poly_t, *poly;
 
void debug_print_poly (char* s, poly p) {
    printf("%s\n", s);
    for (int i = 0; i < p->len; i++) {
        printf("%f,%f\n", p->v[i].x, p->v[i].y);
    }
    printf("\n");
}

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
    f_t dyx = cross(&dy, &dx);
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

