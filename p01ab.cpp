/*  C++  . . .  learning code 
-- Advent of Code 2024 - Day 1 part One and Two
-- Solutions in C++
-- (Ter leering ende vermaeck...)
--
-- The total distance between the lists:     2031679
-- Their similarity score:                  19678534
--
-- (cl) by Arno Jacobs, 2024-12-03
*/

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

vector<int> merge (vector<int> xs, vector<int> ys) {
    if (xs.size() == 0) { return (ys); }
    if (ys.size() == 0) { return (xs); }
    vector <int> mxy;
    if (xs[0]<ys[0]) {
        int x = xs[0];
        xs.erase (xs.begin(),xs.begin()+1);
        mxy = merge (xs,ys);
        mxy.emplace(mxy.begin(),x);
    } else {
        int y = ys[0];
        ys.erase (ys.begin(),ys.begin()+1);
        mxy = merge (xs,ys);
        mxy.emplace(mxy.begin(),y);
    }
    return (mxy);
}

// Merge sort of a "vector<int>" with ~ 43_500 as a maximum size.
// Stack issues?
// Creating a "vector<int>" size 10^9 can be done.
vector<int> mergeSort (vector<int> xs) {
    int lengthXS = xs.size();
    if (lengthXS < 2) { return (xs); }

    // Create two halves
    vector<int> leftHalf;
    vector<int> rightHalf;
    int half = lengthXS / 2; 
    for (int ix=0; ix<half; ix++) {
        leftHalf.push_back(xs[ix]);
    }
    for (int ix=half; ix<lengthXS; ix++) {
        rightHalf.push_back(xs[ix]);
    }
    return (merge(mergeSort(leftHalf),mergeSort(rightHalf)));
}

int tallyVector (int v1, vector<int> vs2) {
    int counter = 0;
    for (int i=0; i < vs2.size(); i++) {
        if (vs2[i] == v1) { counter++; }
    }
    return (counter);
}

int main() { 
    int totalDistances  = 0;
    int similarityScore = 0;
    vector<int> vs1;
    vector<int> vs2;

    ifstream file("data/inputDay01_2024.txt");
    string lines; 
    // Read data and store in two seperate lists
    while (getline(file, lines)) {
        vs1.push_back(stoi(lines));
        vs2.push_back(stoi(lines.substr(7,7)));
    }
    // close file when done
    file.close();
    
    vs1 = mergeSort(vs1);
    vs2 = mergeSort(vs2);
    
    // Part One
    for (int i=0; i < vs1.size(); i++) {
        totalDistances += abs (vs1[i] - vs2[i]);
    }

    // Part Two
    for (int i=0; i < vs1.size(); i++) {
        similarityScore += vs1[i] * tallyVector (vs1[i], vs2);
    }    

    cout << "Advent of Code 2024 - day 1  (C++)" << endl;       
    cout << "Part one: The total distance between the lists:  " << totalDistances   << endl;
    cout << "Part two: Their similarity score:               "  << similarityScore  << endl;
    cout << "0K." << endl << endl;

    return (0);
}

// End of code
