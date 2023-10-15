#include <stdio.h>
#include <stdlib.h>
#include "bmpfile.h"

// BMP defined values for header
#define MAGIC_VALUE     0x4D42
#define BITS_PER_PIXEL  24
#define NUM_PLANE       1
#define COMPRESSION     0
#define BITS_PER_BYTE   8

// return 0 if the header is invalid
// return 1 if the header is valid
static int checkHeader(BMP_Header *hdr) {
    if((hdr->type) != MAGIC_VALUE) {return 0;}
    if((hdr->bits) != BITS_PER_PIXEL) {return 0;}
    if((hdr->planes) != NUM_PLANE) {return 0;}
    if((hdr->compression) != COMPRESSION) {return 0;}
    return 1;
}

// close open file and release memory
BMP_Image *cleanUp(FILE *fptr, BMP_Image *img) {
    if (fptr != NULL) {fclose(fptr);}
    if (img != NULL) {
        if (img->data != NULL) {free(img->data);}
        free(img);
    }
    return NULL;
}

BMP_Image *BMP_open(const char *filename) {
    FILE *fptr = NULL;
    BMP_Image *img = NULL;
    fptr = fopen(filename, "r");
    if (fptr == NULL) {
        return cleanUp(fptr, img);
    }
    // read header
    if (fread(&(img->header), sizeof(BMP_Header),1, fptr) != 1) {
        // fread fails
        return cleanUp(fptr, img);
    }
    if (checkHeader(&(img->header)) == 0) {
        return cleanUp(fptr, img);
    }
    img->data_size          = (img->header).size - sizeof(BMP_Header);
    img->width              = (img->header).width;
    img->height             = (img->header).height;
    img->bytes_per_pixel    = (img->header).bits / BITS_PER_BYTE;
    img->data               = malloc(sizeof(unsigned char) * (img->data_size));
    if((img->data) == NULL) {
        // malloc fail
        return cleanUp(fptr, img);
    }
    if(fread(img->data, sizeof(char), img->data_size, fptr) != (img->data_size)) {
        // fread fails
        return cleanUp(fptr, img);
    }
    // everything successful
    return img;
}

int BMP_save(const BMP_Image *image, const char *filename) {
    FILE *fptr = NULL;
    fptr = fopen(filename, "w");
    if (fptr == NULL) {return 0;}
    // write header first
    if (fwrite(&(image->header), sizeof(BMP_Header), 1, fptr) != 1) {
        // fwrite fails
        fclose(fptr);
        return 0;
    }
    if (fwrite(image->data, sizeof(char), image->data_size, fptr) != image->data_size) {
        // fwrite fails
        fclose(fptr);
        return 0;
    }
    // everything successful
    fclose(fptr);
    return 1;
}

void BMP_destroy(BMP_Image *image) {
    free(image->data);
    free(image);
} 