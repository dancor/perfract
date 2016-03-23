typedef struct { f_t x, y; } vec_t, *vec;

//inline
f_t cross(vec a, vec b)
{
    return a->x * b->y - a->y * b->x;
}
 
//inline
vec vsub(vec a, vec b, vec res)
{
    res->x = a->x - b->x;
    res->y = a->y - b->y;
}
 
// Does point c lie on the left side of directed edge a->b?
// 1 if left, -1 if right, 0 if on the line
int c_left_of_ab(vec a, vec b, vec c)
{
    vec_t tmp1, tmp2;
    f_t x;
    vsub(b, a, &tmp1);
    vsub(c, b, &tmp2);
    x = cross(&tmp1, &tmp2);
    return x < 0 ? -1 : x > 0;
}
