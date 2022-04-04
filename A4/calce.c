#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/**
 * @author @ConorRoberts
 * @date 01/04/2022
 * This program generates e to the specified number of digits.
 */

/**
 * @brief Save the value of e to a file
 *
 * @param e Value of e as string
 * @param fileName Name of file to write to
 */
void keepe(char *e, char *fileName);

int main(int argc, char *argv[])
{
    // Get file name as user input
    char *fileName = (char *)calloc(100, 1);
    printf("Enter file name: ");
    scanf("%s", fileName);

    // Get an integer as user input and assign to n
    int n = 0;
    printf("Enter number of digits: ");
    scanf("%d", &n);

    int m = 4;
    double test = (n + 1) * 2.30258509;

    while ((m * (log(m) - 1) + 0.5 * log(6.2831852 * m)) < test)
        m++;
        
    // Initialize an array of integers of length m to all 1s
    int *coef = (int *)calloc(m + 1, sizeof(int));
    for (int i = 0; i < m + 1; i++)
        coef[i] = 1;

    // Declare a string of length n+2
    char *arr = (char *)calloc(n + 2, 1);
    arr[0] = '2';
    arr[1] = '.';

    for (int i = 0; i < n; i++)
    {
        int carry = 0;
        for (int j = m; j > 1; j--)
        {
            int tmp = coef[j] * 10 + carry;
            carry = tmp / j;
            coef[j] = tmp - carry * j;
        }
        arr[i + 2] = ((int)carry) + 48;
    }

    // Save to file
    keepe(arr, fileName);

    free(arr);
    free(coef);

    return 0;
}

void keepe(char *e, char *fileName)
{
    // Open file
    FILE *file = fopen(fileName, "w+");

    // Check if file exists
    if (file == NULL)
    {
        printf("File does not exist\n");
        exit(1);
    }

    // Write arr to file
    fprintf(file, "%s", e);

    if (file != NULL)
        fclose(file);
}