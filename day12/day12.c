#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    UP,
    DOWN,
    LEFT,
    RIGHT
} Dir;

typedef struct {
    char** grid;
    int numrows;
    int numcols;
} grid_t;

typedef struct {
    int perimeter;
    int area;
    int corners;
} results_t;


int coords_valid(grid_t *grid, int x, int y) {
    return (x >= 0) && (x < grid->numcols) && (y >= 0) && (y < grid->numrows);
}

int is_external(grid_t *grid, Dir dir, int x, int y, char label) {
    int next_x, next_y;
    switch (dir) {
        case UP:
            next_x = x;
            next_y = y - 1;
            break;
        case DOWN:
            next_x = x;
            next_y = y + 1;
            break;
        case LEFT:
            next_x = x - 1;
            next_y = y;
            break;
        case RIGHT:
            next_x = x + 1;
            next_y = y;
            break;
        default:
            next_x = x;
            next_y = y;
            break;
    }

    return !coords_valid(grid, next_x, next_y) || (grid->grid[next_y][next_x] != label);
}

void fill_region(grid_t *orig, grid_t *mut, results_t *res, int x, int y, char label) {
    if (!coords_valid(orig, x, y)) return;
    if (mut->grid[y][x] != label) return;
    
    res->area++;
    
    if (is_external(orig, UP, x, y, label)) res->perimeter++;    
    if (is_external(orig, DOWN, x, y, label)) res->perimeter++;    
    if (is_external(orig, LEFT, x, y, label)) res->perimeter++;    
    if (is_external(orig, RIGHT, x, y, label)) res->perimeter++;    
   
    // Outside corners 
    if (is_external(orig, UP, x, y, label) && is_external(orig, LEFT, x, y, label)) res->corners++;
    if (is_external(orig, UP, x, y, label) && is_external(orig, RIGHT, x, y, label)) res->corners++;
    if (is_external(orig, DOWN, x, y, label) && is_external(orig, LEFT, x, y, label)) res->corners++;
    if (is_external(orig, DOWN, x, y, label) && is_external(orig, RIGHT, x, y, label)) res->corners++;
    
    // Inside corners
    if (!is_external(orig, UP, x, y, label) && !is_external(orig, LEFT, x, y, label)) {
        if (orig->grid[y - 1][x - 1] != label) res->corners++;
    }
    if (!is_external(orig, UP, x, y, label) && !is_external(orig, RIGHT, x, y, label)) {
        if (orig->grid[y - 1][x + 1] != label) res->corners++;
    }
    if (!is_external(orig, DOWN, x, y, label) && !is_external(orig, LEFT, x, y, label)) {
        if (orig->grid[y + 1][x - 1] != label) res->corners++;
    }
    if (!is_external(orig, DOWN, x, y, label) && !is_external(orig, RIGHT, x, y, label)) {
        if (orig->grid[y + 1][x + 1] != label) res->corners++;
    }

    mut->grid[y][x] = '.';

    fill_region(orig, mut, res, x - 1, y, label);
    fill_region(orig, mut, res, x + 1, y, label);
    fill_region(orig, mut, res, x, y - 1, label);
    fill_region(orig, mut, res, x, y + 1, label);
}

int main() {
    FILE* in_file = fopen("input.txt", "r");
    
    grid_t orig_grid, mut_grid;

    char* line;
    int numrows = 0;
    while (fscanf(in_file, "%ms", &line) == 1) numrows++;
    
    orig_grid.numrows = mut_grid.numrows = numrows;

    orig_grid.grid = malloc(sizeof(char*) * numrows);
    mut_grid.grid = malloc(sizeof(char*) * numrows);
    rewind(in_file);
    
    for (int i = 0; i < numrows; i++) {
        fscanf(in_file, "%ms", orig_grid.grid + i);
        mut_grid.grid[i] = malloc(strlen(orig_grid.grid[i]));
        strcpy(mut_grid.grid[i], orig_grid.grid[i]);
    }
    
    int numcols = strlen(orig_grid.grid[0]);
    orig_grid.numcols = mut_grid.numcols = numcols;
    
    int total_cost = 0;
    int total_cost2 = 0;
    for (int y = 0; y < numrows; y++) {
        for (int x = 0; x < numcols; x++) {
            if (mut_grid.grid[y][x] == '.') continue;
            results_t results = {.perimeter = 0, .area = 0, .corners = 0};
            fill_region(&orig_grid, &mut_grid, &results, x, y, mut_grid.grid[y][x]);
            total_cost += results.perimeter * results.area;
            // # vertices == # edges in a 2D shape
            total_cost2 += results.corners * results.area;
        }
    }

    printf("Part 1: %d\n", total_cost);
    printf("Part 2: %d\n", total_cost2);
    return 0;    
}

