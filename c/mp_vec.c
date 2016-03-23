typedef struct { mpq_t x, y; } mp_vec_t, *mp_vec;

void mp_vec_init(mp_vec v)
{
    mpq_init(v->x);
    mpq_init(v->y);
}

void mp_vec_set(mp_vec v, int xn, int xd, int yn, int yd)
{
    mpq_set_si(v->x, xn, xd);
    mpq_set_si(v->y, yn, yd);
}

void mp_vec_init_set(mp_vec v, int xn, int xd, int yn, int yd)
{
    mp_vec_init(v);
    mp_vec_set(v, xn, xd, yn, yd);
}

void mp_vec_clear(mp_vec v)
{
    mpq_clear(v->x);
    mpq_clear(v->y);
}

/*
void mp_vec_to_vec(vec v, const mp_vec mv) {
    v->x = mpq_get_d(mv->x);
    v->y = mpq_get_d(mv->y);
}
*/

void mp_cross(mpq_t res, const mp_vec a, const mp_vec b)
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

void mp_vsub(mp_vec res, mp_vec a, mp_vec b)
{
    mpq_sub(res->x, a->x, b->x);
    mpq_sub(res->y, a->y, b->y);
}

// Does mp_vec c lie on the left side of directed edge a->b?
// 1 if left, -1 if right, 0 if colinear
int mp_c_left_of_ab(mp_vec a, mp_vec b, mp_vec c)
{
    mp_vec_t tmp1, tmp2;
    mpq_t x;
    int res;

    mp_vec_init(&tmp1);
    mp_vec_init(&tmp2);
    mpq_init(x);

    mp_vsub(&tmp1, b, a);
    mp_vsub(&tmp2, c, b);
    mp_cross(x, &tmp1, &tmp2);
    res = mpq_sgn(x);

    mp_vec_clear(&tmp1);
    mp_vec_clear(&tmp2);
    mpq_clear(x);

    return res;
}

void mp_vec_print(const mp_vec v) {
    gmp_printf("%Qd, %Qd\n", v->x, v->y);
}
