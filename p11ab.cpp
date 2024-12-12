/*  C++  . . .  learning code 
-- Advent of Code 2024 - Day 11 part One and Two
-- Solutions in C++
-- (Ter leering ende vermaeck...)
--
--  Part one: number of stones:          189092
--  Part two: number of stones: 224869647102559
--
-- (cl) by Arno Jacobs, 2024-12-12
*/

#include <iostream>
#include <string>
#include <vector>

using namespace std;

// Memoization
vector <uint64_t>   stonesMemory;
vector <int>        blinksMemory;
vector <uint64_t>   valuesMemory;

const uint64_t  notFound    = -1;

uint64_t fromMemory (uint64_t stone, int blinks) {
    uint64_t rv = notFound;
    // for-loop seems to work faster than a while-loop
    for (int c=0; c<stonesMemory.size(); c++) {
        if ((stonesMemory[c] == stone) && (blinksMemory[c] == blinks)) {
            rv = valuesMemory[c];
            break;
        }
    }
    return(rv);
}

void toMemory (uint64_t stone, int blinks, uint64_t value ) {
    stonesMemory.push_back(stone);
    blinksMemory.push_back(blinks);
    valuesMemory.push_back(value);
}

bool evenDigits(string hs) {
    return (hs.length()%2==0);
}

uint64_t leftHalf(string hs,int half) {
    return(stoi(hs.substr(0,half))); // substr first half
}

uint64_t rightHalf(string hs,int half) {
    return(stoi(hs.substr(half))); // substr second half
}

uint64_t solve (uint64_t stone, int blinks) {
    uint64_t value;
    if (blinks == 0) { return (1); }
    else {
        uint64_t rv = fromMemory(stone,blinks);
        if (rv == notFound) {
            if (stone == 0) { 
                value = solve(1,blinks-1); 
            } else {
                string hs = to_string(stone);
                if (evenDigits(hs)) {
                    int half  = hs.length()/2;
                    value = solve(leftHalf(hs,half), blinks-1)
                          + solve(rightHalf(hs,half),blinks-1);
                } else {
                    value = solve(stone*2024,blinks-1);
                }
            }
        } else { return (rv); }
    }
    toMemory(stone,blinks,value);
    return (value);
}

uint64_t workBlinks (vector<uint64_t> numbers,int blinks) {
    uint64_t stoneCount = 0;
    for (int c=0; c<numbers.size(); c++) {
        stoneCount += solve (numbers[c], blinks);
    }
    return(stoneCount);
}

int main() { 
    vector<uint64_t> startNumbers = { 0, 5601550, 3914, 852, 50706, 68, 6, 645371 };
    const int blinksOne = 25;
    const int blinksTwo = 75;

    cout << "Advent of Code 2024 - day 11  (C++)" << endl;       
    cout << "Part one: number of stones after " << blinksOne;
    cout << " blinks:          " << workBlinks(startNumbers,blinksOne) << endl;
    cout << "Part two: number of stones after " << blinksTwo;
    cout << " blinks: " << workBlinks(startNumbers,blinksTwo) << endl;
    cout << "0K." << endl << endl;
    
    return (0);
}

// End of code
