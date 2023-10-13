// This code converts any number to the single precision IEEE 754 standard by retrieving what's stored in the memory
// The hexadecimal number will give the binary representation upon being converted

#include <stdio.h>
#include <stdint.h>


int main(void) {
	float num;
    printf("Enter number: ");
	scanf("%f", &num);
	union {
		float f;
		uint32_t u;
	} f2u = { .f = num };

	printf("Stored number : %f \nIEEE hexadecimal notation : 0x%x\n", num, f2u.u);

	return 0;
}