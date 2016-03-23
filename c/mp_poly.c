typedef struct { int len, alloc; mp_vec v; } mp_poly_t, *mp_poly;

mp_poly mp_poly_new()
{
    return (mp_poly)calloc(1, sizeof(mp_poly_t));
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

/*
void mp_poly_to_poly(poly p, const mp_poly mp) {
    vec_t v;
    for (int i = 0; i < mp->len; i++) {
        mp_vec_to_vec(&v, &mp->v[i]);
        poly_append(p, &v);
    }
}
*/

void mp_poly_print(const mp_poly p) {
    for (int i; i < p->len; i++) {
        mp_vec_print(p->v + i);
    }
}
