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
    mpq_t tmp_c;
    mpq_t tmp_s;
    
    mpq_init(tmp_c);
    mpq_init(tmp_s);

    // a11 = a11 * c - a21 * s
    // a21 = a11 * s + a21 * c
    mpq_mul(tmp_c , a->a11, r->cos);
    mpq_mul(tmp_s , a->a11, r->sin);
    mpq_mul(a->a11, a->a21, r->sin);
    mpq_sub(a->a11, tmp_c , a->a11);
    mpq_mul(a->a21, a->a21, r->cos);
    mpq_add(a->a21, tmp_s , a->a21);

    // a12 = a12 * c - a22 * s
    // a22 = a12 * s + a22 * c
    mpq_mul(tmp_c , a->a12, r->cos);
    mpq_mul(tmp_s , a->a12, r->sin);
    mpq_mul(a->a12, a->a22, r->sin);
    mpq_sub(a->a12, tmp_c , a->a12);
    mpq_mul(a->a22, a->a22, r->cos);
    mpq_add(a->a22, tmp_s , a->a22);

    // ax = ax * c - ay * s
    // ay = ax * s + ay * c
    mpq_mul(tmp_c, a->x , r->cos);
    mpq_mul(tmp_s, a->x , r->sin);
    mpq_mul(a->x , a->y , r->sin);
    mpq_sub(a->x , tmp_c, a->x  );
    mpq_mul(a->y , a->y , r->cos);
    mpq_add(a->y , tmp_s, a->y  );

    mpq_clear(tmp_c);
    mpq_clear(tmp_s);
}

void a_v_apply(mp_vec v, const aug_m a) {
    mpq_t a12y;
    mpq_t a21x;

    mpq_init(a12y);
    mpq_init(a21x);

    // x = a11 * x + a12 * y + ax
    // y = a21 * x + a22 * y + ay
    mpq_mul(a12y, a->a12, v->y);
    mpq_mul(a21x, a->a21, v->x);
    mpq_mul(v->x, a->a11, v->x);
    mpq_add(v->x, v->x, a12y);
    mpq_add(v->x, v->x, a->x);

    mpq_mul(v->y, a->a22, v->y);
    mpq_add(v->y, v->y, a21x);
    mpq_add(v->y, v->y, a->y);

    mpq_clear(a12y);
    mpq_clear(a21x);
}

void a_p_apply(mp_poly p, const aug_m a) {
    for (int i = 0; i < p->len; i++) {
        a_v_apply(&p->v[i], a);
    }
}
