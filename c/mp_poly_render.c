// Required after this function:
// - anything appended to p_new will need an mp_vec_clear
void isect_lines_mp_poly_append(mp_poly p, mp_poly p_new,
    mp_vec x0, mp_vec x1, mp_vec y0, mp_vec y1)
{
    mp_vec_t dx, dy, tmp;
    mpq_t dyx;

    mp_vec_init(&dx);
    mp_vec_init(&dy);
    mpq_init(dyx);

    // x0 + a dx = y0 + b dy ->
    // x0 X dx = y0 X dx + b dy X dx ->
    // b = (x0 - y0) X dx / (dy X dx)
    mp_vsub(&dx, x1, x0);
    mp_vsub(&dy, y1, y0);
    mp_cross(dyx, &dy, &dx);
    // TODO: mpq_equal must be faster?
    if (!mpq_sgn(dyx)) {
        goto isect_lines_mp_poly_append_clear;
    }

    mp_vec_t d;
    mpq_t dyx2;

    mp_vec_init(&d);
    mpq_init(dyx2);

    mp_vsub(&d, x0, y0);
    mp_cross(dyx2, &d, &dx);

    mp_vec_clear(&d);

    mpq_div(dyx, dyx2, dyx);
    if (mpq_cmp_si(dyx, 0, 1) <= 0 || mpq_cmp_si(dyx, 1, 1) >= 0) {
        goto isect_lines_mp_poly_append_clear;
    }
 
    mpq_set(dyx2, dyx);

    mpq_mul(dyx, dyx, dy.x);

    mp_vec_t res;
    mp_vec_init(&res);
    mpq_add(res.x, y0->x, dyx);

    mpq_mul(dyx2, dyx2, dy.y);
    mpq_add(res.y, y0->y, dyx2);
    
    mp_poly_append(p, &res);
    mp_poly_append(p_new, &res);

isect_lines_mp_poly_append_clear:
    
    mp_vec_clear(&dx);
    mp_vec_clear(&dy);
    mpq_clear(dyx);
    mpq_clear(dyx2);
}
 
// Required before a call to this function:
// - res->len = 0
// Required after this function:
// - the mp_vec's appended to to_clear will each need an mp_vec_clear()
void mp_poly_edge_clip(mp_poly res, mp_poly to_clear, mp_poly sub,
        mp_vec clip0, mp_vec clip1)
{
    int i, side0, side1;
    mp_vec v0 = sub->v + sub->len - 1;
    mp_vec v1;

    side0 = mp_c_left_of_ab(clip0, clip1, v0);
    if (side0 != -1) {
        mp_poly_append(res, v0);
    }

    int app = 0;
    for (i = 0; ; i++) {
        v1 = sub->v + i;
        side1 = mp_c_left_of_ab(clip0, clip1, v1);
        if (side0 + side1 == 0 && side0) {
            // last point and current straddle the edge
            isect_lines_mp_poly_append(res, to_clear, clip0, clip1, v0, v1);
        }
        if (i == sub->len - 1) {
            break;
        }
        if (side1 != -1) {
            mp_poly_append(res, v1);
        }
        v0 = v1;
        side0 = side1;
    }
}
 
// Required after this function:
// - the mp_vec's appended to to_clear will each need a mp_vec_clear()
// - the returned mp_poly will need an mp_poly_free()
mp_poly mp_poly_clip(mp_poly to_clear, mp_poly sub, mp_poly clip)
{
    mp_poly p1 = mp_poly_new();
    mp_poly p2 = mp_poly_new();
    mp_poly tmp;
 
    mp_poly_edge_clip(p2, to_clear, sub, clip->v + clip->len - 1, clip->v);
    for (int i = 0; i < clip->len - 1; i++) {
        tmp = p2; p2 = p1; p1 = tmp;

        p2->len = 0;
        if (p1->len == 0) {
            break;
        }
        mp_poly_edge_clip(p2, to_clear, p1, clip->v + i, clip->v + i + 1);
    }
 
    mp_poly_free(p1);
    return p2;
}

void mp_poly_area(mpq_t area, mp_poly p)
{
    mpq_t x1y2;
    mpq_t x2y1;

    mpq_init(x1y2);
    mpq_init(x2y1);

    int i = p->len - 1;
    mpq_mul(x1y2, p->v[i].x, p->v[0].y);
    mpq_mul(x2y1, p->v[0].x, p->v[i].y);
    mpq_sub(area, x1y2, x2y1);
    for (i = 0; i < p->len - 1; i++) {
        int i2 = i + 1;
        mpq_mul(x1y2, p->v[i].x, p->v[i2].y);
        mpq_mul(x2y1, p->v[i2].x, p->v[i].y);
        mpq_sub(x1y2, x1y2, x2y1);
        mpq_add(area, area, x1y2);
    }
    mpq_set_si(x1y2, 1, 2);
    mpq_mul(area, area, x1y2);

    mpq_clear(x1y2);
    mpq_clear(x2y1);
}

void mp_poly_render(bitmap_t *b, mp_poly p) {
    mpq_t area;
    mpz_t mpz_area;
    uint32_t area_res;
    pixel_t *cur_pixel = b->pixels;
    mpq_t mpq_1_2;
    mpq_t mpq_255;
    mp_poly pixel_poly = mp_poly_new();
    mp_vec_t v1;
    mp_vec_t v2;
    mp_vec_t v3;
    mp_vec_t v4;
    mp_vec_init(&v1);
    mp_vec_init(&v2);
    mp_vec_init(&v3);
    mp_vec_init(&v4);
    int x2;
    int y2;
    mp_poly to_clear = mp_poly_new();

    mpq_init(area);
    mpz_init(mpz_area);
    mpq_init(mpq_1_2);
    mpq_set_si(mpq_1_2, 1, 2);
    mpq_init(mpq_255);
    mpq_set_si(mpq_255, 255, 1);
    mp_poly_append(pixel_poly, &v1);
    mp_poly_append(pixel_poly, &v2);
    mp_poly_append(pixel_poly, &v3);
    mp_poly_append(pixel_poly, &v4);
    for (int y = 0; y < b->h; y++) {
        for (int x = 0; x < b->w; x++) {
            mp_vec_set(pixel_poly->v    , x    , 1, y    , 1);
            mp_vec_set(pixel_poly->v + 1, x + 1, 1, y    , 1);
            mp_vec_set(pixel_poly->v + 2, x + 1, 1, y + 1, 1);
            mp_vec_set(pixel_poly->v + 3, x    , 1, y + 1, 1);

            mp_poly res = mp_poly_clip(to_clear, p, pixel_poly);
            if (res->len) {
                mp_poly_area(area, res);
                mpq_mul(area, area, mpq_255);
                mpq_add(area, area, mpq_1_2);
                mpz_fdiv_q(mpz_area, mpq_numref(area), mpq_denref(area));
                area_res = mpz_get_ui(mpz_area);
                if (area_res < 0 || area_res > 255) {
                    printf("%d\n", area_res);
                    gmp_printf("%Qd\n", area);
                    mp_poly_print(res);
                    printf("\n");
                    exit(-1);
                }
                cur_pixel->r = area_res;
            }
            cur_pixel++;
        }
    }

    for (int i = 0; i < to_clear->len; i++) {
        mp_vec_clear(to_clear->v + i);
    }
    mpq_clear(area);
    mpz_clear(mpz_area);
    mpq_clear(mpq_1_2);
    mpq_clear(mpq_255);
    mp_poly_free(pixel_poly);
    mp_vec_clear(&v1);
    mp_vec_clear(&v2);
    mp_vec_clear(&v3);
    mp_vec_clear(&v4);
}
