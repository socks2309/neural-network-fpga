// Get the binary representation of any number in the IEEE format. 

// Code uses ISO C99 and will probably cause issues if compiled with MSVC. 
// Use Clang or GCC for compilation.

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define SINGLE_PREC__MANTISSA_SIZE 23
#define SINGLE_PREC__SIZE 32
#define SINGLE_PREC__EXP_SIZE 8
#define SINGLE_PREC__EXP_MAX_VAL 127
#define __STDC_WANT_LIB_EXT1__ 1

struct bit___array {
    uint8_t bit : 1;
};

struct array__bits {
    struct bit___array array[SINGLE_PREC__SIZE];
};

struct bin___array {
    int* arrP;
    int size;
};

void print_num_array(int* arr, int start, int end);
void print_struct_array(struct array__bits array, int start, int end);
void reverse(int* array, int size);
struct bin___array int_to_bin(int int_part);
struct bin___array dec_to_bin(float dec_part);
struct array__bits combine__arr(struct bin___array int__arr, struct bin___array dec__arr);
struct array__bits ieee754__convert(struct bin___array int__arr, struct bin___array dec__arr, int zero__int);
struct array__bits ieee754__format(float num);
int fileIsEmpty(FILE* fptr);
void saveToFile(struct array__bits array);

int main() {
    clock_t tic = clock();
    
    float num;
    printf("Enter number you want the IEEE 754 format of: ");
    scanf("%f", &num);
    
    struct array__bits ieee754array = ieee754__format(num);
    printf("The IEEE754 format of entered number: ");
    print_struct_array(ieee754array, 0, SINGLE_PREC__SIZE);

    saveToFile(ieee754array);

    clock_t toc = clock();
    double duration = ((double)(toc - tic)) / CLOCKS_PER_SEC;
    printf("\nDURATION OF PROGRAM = %f\n", duration);
}

struct array__bits ieee754__format(float number) {
    float num = fabs(number);
    int zero__int = 0;
    int int__part = (int)num;
    float dec__part = num - int__part;

    if (int__part == 0) {
        zero__int = 1;
    }
    else zero__int = 0;

    struct bin___array int__array = int_to_bin(int__part);
    struct bin___array dec__array = dec_to_bin(dec__part);
    struct array__bits ieee754__array = ieee754__convert(int__array, dec__array, zero__int);
    if (number < 0) {
        ieee754__array.array[0].bit = 1;
    }
    else ieee754__array.array[0].bit = 0;
    return ieee754__array;
}

struct bin___array int_to_bin(int int_part) {
    struct bin___array int__arr;
    int* temp_arr = (int*)calloc(SINGLE_PREC__MANTISSA_SIZE, sizeof(int));
    int index = 0;
    int__arr.size = 0;
    while (int_part != 0) {
        temp_arr[index] = int_part % 2;
        int_part = int_part / 2;
        index++;
        int__arr.size++;
    }
    int__arr.arrP = (int*)calloc(int__arr.size, sizeof(int));
    for (int i = 0; i < int__arr.size; i++) {
        int__arr.arrP[i] = temp_arr[i];
    }
    reverse(int__arr.arrP, int__arr.size);
    free(temp_arr);
    return int__arr;
}

struct bin___array dec_to_bin(float dec_part) {
    struct bin___array dec__arr;
    dec__arr.arrP = (int*)calloc(SINGLE_PREC__SIZE, sizeof(int));
    dec__arr.size = 0;
    for (int i = 0; i < SINGLE_PREC__SIZE; i++) {
        dec_part = dec_part * 2;
        if ((int)dec_part == 1) {
            dec__arr.arrP[i] = 1;
            dec_part = dec_part - 1;
        }
        else dec__arr.arrP[i] = 0;
        dec__arr.size++;
    }
    return dec__arr;
}

struct array__bits combine__arr(struct bin___array int__arr, struct bin___array dec__arr) {
    struct array__bits mantissa_arr;
    int index, count = 0;
    for (index = 0; index < int__arr.size; index++) {
        mantissa_arr.array[index].bit = int__arr.arrP[index];
        count++;
    }
    for (index = int__arr.size; index < dec__arr.size; index++) {
        mantissa_arr.array[index].bit = dec__arr.arrP[index - int__arr.size];
        count++;
    }
    return mantissa_arr;
}

struct array__bits ieee754__convert(struct bin___array int__arr, struct bin___array dec__arr, int zero__int) {
    struct array__bits ieee754_converted;
    if (zero__int == 1) {
        struct array__bits temp__arr = combine__arr(int__arr, dec__arr);
        int index = 0, count = 0;
        while (index < SINGLE_PREC__SIZE) {
            if (temp__arr.array[index].bit == 1) {
                count++;
                break;
            }
            else {
                index++;
                count++;
            }
        }
        int ctr = count;
        for (int i = 0; i < SINGLE_PREC__SIZE - ctr; i++) {
            temp__arr.array[i].bit = temp__arr.array[i + ctr].bit;
        }
        int exponent = SINGLE_PREC__EXP_MAX_VAL - count;
        struct bin___array exponent__arr = int_to_bin(exponent);
        ieee754_converted.array[1].bit = 0;
        for (int k = 2; k <= SINGLE_PREC__EXP_SIZE; k++) {
            ieee754_converted.array[k].bit = exponent__arr.arrP[k-2];
        }

        for (int l = SINGLE_PREC__EXP_SIZE + 1; l < SINGLE_PREC__SIZE; l++) {
            ieee754_converted.array[l].bit = temp__arr.array[l - SINGLE_PREC__EXP_SIZE - 1].bit;
        }
    }
    else {
        struct array__bits temp_arr = combine__arr(int__arr, dec__arr);
        for (int i = 0; i < SINGLE_PREC__SIZE - 1; i++) {
            temp_arr.array[i].bit = temp_arr.array[i + 1].bit;
        }
        int exponent = SINGLE_PREC__EXP_MAX_VAL + (int__arr.size - 1);
        struct bin___array exponent__arr = int_to_bin(exponent);
        if (exponent__arr.size == 7) {
            ieee754_converted.array[1].bit = 0;
            for (int j = 2; j <= SINGLE_PREC__EXP_SIZE; j++) {
                ieee754_converted.array[j].bit = exponent__arr.arrP[j - 2];
            }
        }
        else {
            for (int j = 1; j <= SINGLE_PREC__EXP_SIZE; j++) {
                ieee754_converted.array[j].bit = exponent__arr.arrP[j - 1];
            }
        }
        for (int k = SINGLE_PREC__EXP_SIZE + 1; k < SINGLE_PREC__SIZE; k++) {
            ieee754_converted.array[k].bit = temp_arr.array[k - SINGLE_PREC__EXP_SIZE - 1].bit;
        }
    }
    return ieee754_converted;
}

void reverse(int* array, int size) {
    int* temp = (int*)calloc(size, sizeof(int));
    for (int i = 0; i < size; i++) {
        temp[i] = array[i];
    }
    for (int k = size - 1; k >= 0; k--) {
        array[k] = temp[size - 1 - k];
    }
    free(temp);
}

void print_struct_array(struct array__bits array, int start, int end) {
    for (int i = start; i < end; i++) {
        printf("%d", array.array[i].bit);
    }
}

void print_num_array(int* arr, int start, int end) {
    for (int i = start; i < end; i++) {
        printf("%d", arr[i]);
    }
}

int fileIsEmpty(FILE* fptr) {
    fseek(fptr,0,SEEK_END);
    int size = ftell(fptr), isEmpty = 0;
    if (0 == size) {
        isEmpty = 1;
    }
    else isEmpty = 0;
    return isEmpty;
}

void saveToFile(struct array__bits bit__arr) {
    FILE* fptr;
    fptr = fopen("file.txt","w+");
    for(int i = 0; i < SINGLE_PREC__SIZE; i++) {
        fprintf(fptr, "%d", bit__arr.array[i].bit);
    }
    fclose(fptr);
}
