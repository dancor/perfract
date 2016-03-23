#include <png.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
    
typedef struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
} pixel_t;

typedef struct  {
    pixel_t *pixels;
    size_t w;
    size_t h;
} bitmap_t;
    
pixel_t* pixel_at(bitmap_t*, int, int);
int save_png_to_file(bitmap_t*, const char *);
void bitmap_init(bitmap_t*, int, int);
void bitmap_blackout(bitmap_t*);
void bitmap_free(bitmap_t*);
