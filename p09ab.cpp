/*  C++  . . .  learning code 
-- Advent of Code 2024 - Day 9 part One and Two
-- Solutions in C++
-- (Ter leering ende vermaeck...)
--
--  Part one: Resulting filesystem checksum: 6471961544878
--  Part two: Resulting filesystem checksum: 6511178035564
--
-- (cl) by Arno Jacobs, 2024-12-11
*/

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

const int freeSpace = -1;

vector <int> parseDiskFormat (string diskFormat) {
    vector<int> newFile;
    int fileCount = 0;
    int fileSize;

    for (int pc=0; pc<diskFormat.length(); pc++) {
        fileSize = diskFormat[pc] - '0'; 
        if (pc % 2 == 1) { 
            fileCount = freeSpace;
        } else {
            fileCount   = pc / 2;
        }
        for (int c=0; c<fileSize; c++) {
            newFile.push_back(fileCount);
        } 
    }
    return (newFile);    
}

uint64_t checkSum (vector <int> disk) {
    uint64_t csum = 0;
    for (int c=0; c < disk.size(); c++) {
        if (disk[c] > freeSpace) {
            csum += c * disk[c];
        }
    }
    return (csum);
}


// ---- Part one ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

vector <int> compact (vector <int> disk) {
    int filePart;
    for (int c=disk.size()-1; c > 0; c--) {
        filePart = disk[c];
        if (filePart > freeSpace) {
            for (int p=0; p < c; p++) {
                if (disk[p] < 0) {
                    disk[p] = filePart;
                    disk[c] = freeSpace;
                    break;
                }
            }
        }
    }    
    return (disk);
}

// Compact the disk 
uint64_t partOne (string diskFormat) {
    vector <int> cd = compact( parseDiskFormat(diskFormat) );
    return ( checkSum(cd) );
}


// ---- Part two ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

vector <int> defrag (vector <int> disk) {

    for (int c=disk.size()-1; c>0; c--) {
        int filePart = disk[c];
        if (filePart > freeSpace) {
            // First get block start position and block size
            // Looking from end to start of the disk
            int bStart = c;
            while (disk[bStart] == filePart) { bStart--; }
            int bSize = c - bStart;
            bStart++;
            // Next find free space start position with minimal block size
            bool fsFound = false;
            int fp = 0;
            int fbStart;
            // Looking for free space from start to end of the disk
            while ((!fsFound) && (fp <= bStart)) {
                while (disk[fp] != freeSpace) { fp++; }
                fbStart = fp;
                while (disk[fp] == freeSpace) { fp++; }
                fsFound = bSize <= (fp-fbStart);
            }
            // Then swap complete file block with free space
            // If free space block is found then swap file block with free block 
            if ((fsFound) && (bStart > fbStart)) {
                for (fp=0; fp<bSize; fp++) {
                    disk[fbStart+fp] = filePart;   
                    disk[ bStart+fp] = freeSpace;   
                }
            }
            c = bStart;     // Skip the tested block
        }
    }    
    return (disk);
}


// Defrag the disk
uint64_t partTwo (string diskFormat) {
    vector <int> dd = defrag( parseDiskFormat(diskFormat) );
    return ( checkSum(dd) );    
}

int main() { 
    ifstream file("data/inputDay09_2024.txt");
    string diskFormat; 
    // Read data and store in two seperate lists
    getline(file, diskFormat);
    // close file when done
    file.close();

    cout << "Advent of Code 2024 - day 9  (C++)" << endl;       
    cout << "Part one: Resulting filesystem checksum: " << partOne(diskFormat) << endl;
    cout << "Part two: Resulting filesystem checksum: " << partTwo(diskFormat) << endl;
    cout << "0K." << endl << endl;

    return (0);
}

// End of code
