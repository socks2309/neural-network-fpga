// update:
#include <stdio.h>
#include <stdlib.h>

const int nrow = 8, ncol = 8;
const int kernel_size = 3;

// Function to create the CNN kernel with max pooling

void *cnn_kernel(int data[nrow][ncol]) {
  static int cached_data[50][50];
  static int maxpool[20]; // buffer to store pooled values
  // check dimension of row and column and padding zero if required
  int n_pad_zero_row;
  int n_pad_zero_col;
  int i, j;
  if (nrow % kernel_size != 0)
    n_pad_zero_row = kernel_size - (nrow % kernel_size);
  if (ncol % kernel_size != 0)
    n_pad_zero_col = kernel_size - (ncol % kernel_size);

  // printf("%d , %d\n",n_pad_zero_row,n_pad_zero_col);
  // create a new array and copy original array content along with padded zeros

  // appending zero in last row and column
  for (i = 0; i < nrow + n_pad_zero_row; i++) {
    for (j = 0; j < ncol + n_pad_zero_col; j++) {
      if (i < nrow && j < ncol)
        cached_data[i][j] = data[i][j];
      else
        cached_data[i][j] = 0;
    }
  }

  // print catched matrix before feeding to cnn filter
  printf("Modified array with appended zero->\n");
  for (i = 0; i < nrow + n_pad_zero_row; i++) {
    for (j = 0; j < ncol + n_pad_zero_col; j++) {
      printf("\t%d", cached_data[i][j]);
    }
    printf("\n\n");
  }

  //
  // int n_element = (nrow - 1) * (nrow - 1);
  // int maxpool[n_element];
  int rstart = 0;
  int rstop = kernel_size;
  int cstart = 0;
  int cstop = kernel_size;
  int tmp = 0;
  int indx = 0;
  // printf("CNN Filter and Max pooling result->\n");
  for (int jj = 0; jj < (nrow + n_pad_zero_row) / kernel_size; jj++) {
    for (int ii = 0; ii < (ncol + n_pad_zero_col) / kernel_size; ii++) {
      for (i = rstart; i < rstop; i++) {
        for (j = cstart; j < cstop; j++) {
          // printf("\t %d", cached_data[i][j]);
          // pull max value
          if (cached_data[i][j] > tmp)
            tmp = cached_data[i][j];
        }
        // printf("\n");
      }
      // printf("Max val-> %d\n\n",tmp);
      // store result
      maxpool[indx] = tmp;
      indx++;
      // shift the kernel block
      cstart += kernel_size;
      cstop += kernel_size;
      tmp = 0;
      // return tmp;
    }

    rstart += kernel_size;
    rstop += kernel_size;
    cstart = 0;
    cstop = kernel_size;
    tmp = 0;
  }
  return maxpool;
}

// Main function to test

int main() {
  int *res;
  // declare the local variables

  // declare the 2d array
  int x[8][8] = {{1, 2, 8, 1, 5, 0, 11, 3},   {10, 4, 5, 7, 1, 0, 3, 9},
                 {1, 5, 2, 9, 4, 5, 0, 4},    {3, 2, 5, 0, 8, 1, 6, 4},
                 {0, 1, 1, 2, 5, 9, 2, 1},    {3, 3, 2, 1, 6, 7, 1, 5},
                 {12, 3, 9, 0, 8, 10, 11, 5}, {1, 1, 5, 9, 3, 10, 1, 6}};

  // result = cnn_kernel(x);
  res = cnn_kernel(x);
  // printf("Pooled data : %d\n",result);
  printf("\nMax pooled values are->\n");
  for (int i = 0; i < (kernel_size * kernel_size); i++) {
    printf(" %d\n", *(res + i));
  }
  return 0;
}
