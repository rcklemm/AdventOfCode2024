#include <stdio.h>
#include <stdlib.h>
#include <string.h>


// compute how big of a disk the string represents
size_t buf_len_of_str(char* str) {
    size_t sum = 0;
    for (size_t i = 0 ; i < strlen(str) ; i++) {
        sum += (str[i] - '0');
    }
    return sum;
}

// given the string, fill the disk buffer with ids and free blocks
void fill_buf(int* disk_buf, char* str) {
    int id = 0;
    for (size_t i = 0 ; i < strlen(str) ; i++) {
        // Every other number in the map is allocated / free
        if (i % 2 == 0) {
            for (int j = 0 ; j < (str[i] - '0') ; j++) {
                *disk_buf = id;
                disk_buf++;
            }
            id++;
        } 
        else {
            for (int j = 0; j < (str[i] - '0') ; j++) {
                *disk_buf = -1;
                disk_buf++;
            }
        }
    }
}

// Remove free space in the buffer, return new length
size_t pack_buf(int* disk_buf, size_t len) {
    int* fwd = disk_buf;
    int* bck = disk_buf + len - 1;
    while (bck > fwd) {
        // find next free space
        while (*fwd >= 0) fwd++;
        // find next non-free block
        while (*bck < 0) bck--;
        // swap, unless done already
        if (fwd >= bck) break;
        *fwd = *bck;
        *bck = -1;
        // Advance pointers again
        fwd++;
        bck--;
    }
    // find leftmost blank spot
    while (*fwd >= 0) fwd++;
    // will always be non-negative
    return (size_t) (fwd - disk_buf);
}

// checksum is sum(pos*id) across the packed buffer
long calc_checksum(int* disk_buf, size_t len) {
    long sum = 0;
    for (size_t i = 0; i < len; i++) {
        if (disk_buf[i] < 0) continue;
        sum += (i * disk_buf[i]);
    }
    return sum;
}

size_t pack_buf2(int* disk_buf, size_t len) {
    int* bck = disk_buf + len - 1;
    // find the first file block
    while (*bck < 0) bck--;
    
    int id = *bck;
    while (id > 0) {
        int file_len = 0;
        while (*bck == id) { file_len++; bck--; }
        // Find the next block of free space that this file fits in,
        // but before any files we've already scanned
        int space_found = 0;
        int* space = disk_buf;
        while (space < bck) {
            if (*space >= 0) { space++; continue; }
            int space_len = 0;
            while (*space < 0) { space_len++; space++; }
            if (space_len >= file_len) { space -= space_len; space_found = 1; break; }
        }
        // place file in its new spot, clear old spot
        if (space_found) {
            for (int i = 0; i < file_len; i++) {
                space[i] = id;
                bck[i + 1] = -1;
            }
        }
        // find the next file block
        id--;
        while (*bck != id) bck--;
    }
    // Scan for end of list
    bck = disk_buf + len - 1;
    while (*bck < 0) bck--;
    // always non-negative
    return (size_t) (bck - disk_buf + 1);
}

int main() {
    FILE* in_file = fopen("input.txt", "r");
    char* in_str;
    fscanf(in_file, "%ms", &in_str);
    
    size_t buf_len = buf_len_of_str(in_str);
    int* disk_buf = malloc(sizeof(int) * buf_len);
    
    fill_buf(disk_buf, in_str);
    size_t packed_len = pack_buf(disk_buf, buf_len);    
    
    long checksum = calc_checksum(disk_buf, packed_len);

    printf("Part 1: %ld\n", checksum);
    

    // Reset buffer for part 2
    fill_buf(disk_buf, in_str);
    size_t packed_len2 = pack_buf2(disk_buf, buf_len);
    
    long checksum2 = calc_checksum(disk_buf, packed_len2);

    printf("Part 2: %ld\n", checksum2);

    return 0;    
}
