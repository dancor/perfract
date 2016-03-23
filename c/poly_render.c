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

f_t poly_area(poly p) {
    int i_last = p->len - 1;
    f_t sum = p->v[i_last].x * p->v[0].y - p->v[0].x * p->v[i_last].y;
    for (int i = 0;;) {
        int i2 = i + 1;
        sum += p->v[i].x * p->v[i2].y - p->v[i2].x * p->v[i].y;
        if (i2 == i_last) break;
        i = i2;
    }
    return 0.5f * sum;
}

void poly_render(bitmap_t *b, poly p) {
    vec_t v1, v2, v3, v4;
    poly pixel_poly = poly_new();
    f_t xf;
    f_t xf2;
    f_t yf;
    f_t yf2;
    pixel_t *cur_pixel = b->pixels;

    poly_append(pixel_poly, &v1);
    poly_append(pixel_poly, &v2);
    poly_append(pixel_poly, &v3);
    poly_append(pixel_poly, &v4);
    for (int y = 0, yf = 0; y < b->h; y++) {
        yf2 = yf + 1.0f;
        for (int x = 0, xf = 0; x < b->w; x++) {
            xf2 = xf + 1.0f;
            v1.x = xf;
            v1.y = yf;
            v2.x = xf2;
            v2.y = yf;
            v3.x = xf2;
            v3.y = yf2;
            v4.x = xf;
            v4.y = yf2;
            poly res = poly_clip(p, pixel_poly);
            if (res->len) {
                f_t area_f = poly_area(poly_clip(p, pixel_poly));
                int area =
                    (int)(poly_area(poly_clip(p, pixel_poly)) * 255.0f + 0.5);
                assert(area > 0);
                if (area > 255) {
                    printf("%d,%d: %d\n", x, y, area);
                    printf("%f\n", area_f);
                    printf("len: %d\n", res->len);
                    for (int i = 0; i < res->len; i++) {
                        printf("%f, %f\n", res->v[i].x, res->v[i].y);
                    }
                }
                assert(area <= 255);
                cur_pixel->r = area;
            }
            xf = xf2;
            cur_pixel++;
        }
        yf = yf2;
    }
    poly_free(pixel_poly);
}
