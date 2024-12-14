#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void str_to_int_grid(char** strgrid, int** intgrid, int rows, int cols) {
    for (int x = 0; x < cols; x++) {
        for (int y = 0; y < rows; y++) {
            intgrid[x][y] = strgrid[y][x] - '0';
        }
    }
}

int* trails;
int numtrails = 0;

int count_trailheads(int** grid, int x, int y, int rows, int cols, int num) {
    if ((x < 0) || (x >= cols) || (y < 0) || (y >= rows)) return 0;
    if (grid[x][y] != num) return 0;
    if (num == 9) {
        for (int i = 0; i < numtrails; i++) {
           if ((trails[2*i] == x) && (trails[2*i + 1] == y)) {
                return 0;
            }
        }
        trails[2*numtrails] = x;
        trails[2*numtrails + 1] = y;
        numtrails++;
        return 1;
    }

    return count_trailheads(grid, x - 1, y, rows, cols, num + 1)
        + count_trailheads(grid, x + 1, y, rows, cols, num + 1)
        + count_trailheads(grid, x, y - 1, rows, cols, num + 1)
        + count_trailheads(grid, x, y + 1, rows, cols, num + 1);
}

int count_trailheads2(int** grid, int x, int y, int rows, int cols, int num) {
    if ((x < 0) || (x >= cols) || (y < 0) || (y >= rows)) return 0;
    if (grid[x][y] != num) return 0;
    if (num == 9) return 1;

    return count_trailheads2(grid, x - 1, y, rows, cols, num + 1)
        + count_trailheads2(grid, x + 1, y, rows, cols, num + 1)
        + count_trailheads2(grid, x, y - 1, rows, cols, num + 1)
        + count_trailheads2(grid, x, y + 1, rows, cols, num + 1);
}

int main() {
    FILE* in_file = fopen("input.txt", "r");
    
    char* line;
    int numrows = 0;
    while (fscanf(in_file, "%ms", &line) == 1) numrows++;

    char** strgrid = malloc(sizeof(char*) * numrows);
    rewind(in_file);
    
    for (int i = 0; i < numrows; i++) fscanf(in_file, "%ms", strgrid + i);
    
    int numcols = strlen(strgrid[0]);
    int** grid = malloc(sizeof(int*) * numcols);
    for (int i = 0; i < numcols; i++) grid[i] = malloc(sizeof(int) * numrows);

    str_to_int_grid(strgrid, grid, numrows, numcols);
    
    trails = malloc(sizeof(int) * numrows * numcols * 2);
    int total_trails = 0;
    for (int x = 0; x < numcols; x++) {
        for (int y = 0; y < numrows; y++) {
            if (grid[x][y] != 0) continue;
            total_trails += count_trailheads(grid, x, y, numrows, numcols, 0);
            numtrails = 0;
        }
    }

    printf("Part 1: %d\n", total_trails);
    
    total_trails = 0;
    for (int x = 0; x < numcols; x++) {
        for (int y = 0; y < numrows; y++) {
            if (grid[x][y] != 0) continue;
            total_trails += count_trailheads2(grid, x, y, numrows, numcols, 0);
        }
    } 
    
    printf("Part 2: %d\n", total_trails); 
    return 0;    
}

