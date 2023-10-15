#ifndef _BMPHEADER_H
#define _BMPHEADER_H
#include <stdint.h>
#pragma pack(1) // tell compiler to not add space between attributes

// Bitmap header
typedef struct Header {
    uint16_t type;                  // Magic identifier
    uint32_t size;                  // File size in bytes
    uint16_t reserved1;             // Not used
    uint16_t reserved2;             // Not used
    uint32_t offset;                //
    uint32_t header_size;           // Header size in bytes
    uint32_t width;                 // Width of the image
    uint32_t height;                // Height of image
    uint16_t planes;                // Number of color planes
    uint16_t bits;                  // Bits per pixel
    uint32_t compression;           // Compression type
    uint32_t imagesize;             // Image size in bytes
    uint32_t xresolution;           // Pixels per meter
    uint32_t yresolution;           // Pixels per meter
    uint32_t ncolors;               // Number of colors
    uint32_t importantcolors;       // Important colors
} BMP_Header;

#endif