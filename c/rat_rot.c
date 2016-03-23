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
