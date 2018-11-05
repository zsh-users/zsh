#include "nearcolor.mdh"
#include "nearcolor.pro"

#include <math.h>

struct cielab {
    float L, a, b;
};
typedef struct cielab *Cielab;

static float
deltae(Cielab lab1, Cielab lab2)
{
    /* taking square root unnecessary as we're just comparing values */
    return powf(lab1->L - lab2->L, 2) +
	powf(lab1->a - lab2->a, 2) +
	powf(lab1->b - lab2->b, 2);
}

static void
RGBtoLAB(int red, int green, int blue, Cielab lab)
{
    float R = (float)red / 255.0;
    float G = (float)green / 255.0;
    float B = (float)blue / 255.0;
    R = 100.0 * (R > 0.04045 ? powf((R + 0.055) / 1.055, 2.4) : R / 12.92);
    G = 100.0 * (G > 0.04045 ? powf((G + 0.055) / 1.055, 2.4) : G / 12.92);
    B = 100.0 * (B > 0.04045 ? powf((B + 0.055) / 1.055, 2.4) : B / 12.92);

    /* Observer. = 2 degrees, Illuminant = D65 */
    float X = (R * 0.4124 + G * 0.3576 + B * 0.1805) / 95.047;
    float Y = (R * 0.2126 + G * 0.7152 + B * 0.0722) / 100.0;
    float Z = (R * 0.0193 + G * 0.1192 + B * 0.9505) / 108.883;

    X = (X > 0.008856) ? powf(X, 1.0/3.0) : (7.787 * X) + (16.0 / 116.0);
    Y = (Y > 0.008856) ? powf(Y, 1.0/3.0) : (7.787 * Y) + (16.0 / 116.0);
    Z = (Z > 0.008856) ? powf(Z, 1.0/3.0) : (7.787 * Z) + (16.0 / 116.0);

    lab->L = (116.0 * Y) - 16.0;
    lab->a = 500.0 * (X - Y);
    lab->b = 200.0 * (Y - Z);
}

static int
mapRGBto88(int red, int green, int blue)
{
    int component[] = { 0, 0x8b, 0xcd, 0xff, 0x2e, 0x5c, 0x8b, 0xa2, 0xb9, 0xd0, 0xe7 };
    struct cielab orig, next;
    float nextl, bestl = -1;
    int r, g, b;
    int comp_r = 0, comp_g = 0, comp_b = 0;

    /* Get original value */
    RGBtoLAB(red, green, blue, &orig);

    /* try every one of the 72 colours */
    for (r = 0; r < 11; r++) {
	for (g = 0; g <= 3; g++) {
	    for (b = 0; b <= 3; b++) {
		if (r > 3) g = b = r; /* advance inner loops to the block of greys */
		RGBtoLAB(component[r], component[g], component[b], &next);
		nextl = deltae(&orig, &next);
		if (nextl < bestl || bestl < 0) {
		    bestl = nextl;
		    comp_r = r;
		    comp_g = g;
		    comp_b = b;
		}
	    }
	}
    }

    return (comp_r > 3) ? 77 + comp_r :
        16 + (comp_r * 16) + (comp_g * 4) + comp_b;
}

/*
 * Convert RGB to nearest colour in the 256 colour range
 */
static int
mapRGBto256(int red, int green, int blue)
{
    int component[] = {
	0, 0x5f, 0x87, 0xaf, 0xd7, 0xff,
	0x8, 0x12, 0x1c, 0x26, 0x30, 0x3a, 0x44, 0x4e,
	0x58, 0x62, 0x6c, 0x76, 0x80, 0x8a, 0x94, 0x9e,
	0xa8, 0xb2, 0xbc, 0xc6, 0xd0, 0xda, 0xe4, 0xee
    };
    struct cielab orig, next;
    float nextl, bestl = -1;
    int r, g, b;
    int comp_r = 0, comp_g = 0, comp_b = 0;

    /* Get original value */
    RGBtoLAB(red, green, blue, &orig);

    for (r = 0; r < sizeof(component)/sizeof(*component); r++) {
	for (g = 0; g <= 5; g++) {
	    for (b = 0; b <= 5; b++) {
		if (r > 5) g = b = r; /* advance inner loops to the block of greys */
		RGBtoLAB(component[r], component[g], component[b], &next);
		nextl = deltae(&orig, &next);
		if (nextl < bestl || bestl < 0) {
		    bestl = nextl;
		    comp_r = r;
		    comp_g = g;
		    comp_b = b;
		}
	    }
	}
    }

    return (comp_r > 5) ? 226 + comp_r :
	16 + (comp_r * 36) + (comp_g * 6) + comp_b;
}

static int
getnearestcolor(UNUSED(Hookdef dummy), Color_rgb col)
{
    if (tccolours == 256)
	return mapRGBto256(col->red, col->green, col->blue);
    if (tccolours == 88)
	return mapRGBto88(col->red, col->green, col->blue);
    return 0;
}

static struct features module_features = {
    NULL, 0,
    NULL, 0,
    NULL, 0,
    NULL, 0,
    0
};

/**/
int
setup_(UNUSED(Module m))
{
    return 0;
}

/**/
int
features_(Module m, char ***features)
{
    *features = featuresarray(m, &module_features);
    return 0;
}

/**/
int
enables_(Module m, int **enables)
{
    return handlefeatures(m, &module_features, enables);
}

/**/
int
boot_(Module m)
{
    addhookfunc("get_color_attr", (Hookfn) getnearestcolor);
    return 0;
}

/**/
int
cleanup_(Module m)
{
    deletehookfunc("get_color_attr", (Hookfn) getnearestcolor);
    return setfeatureenables(m, &module_features, NULL);
}

/**/
int
finish_(UNUSED(Module m))
{
    return 0;
}
