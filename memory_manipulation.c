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