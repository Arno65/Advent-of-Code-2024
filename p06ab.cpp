/*  C++  . . .  learning code 
-- Advent of Code 2024 - Day 6 part One and Two
-- Solutions in C++
-- (Ter leering ende vermaeck...)
--
--  Part one: Number of distinct positions:  4988
--  Part two: Number of different positions: 1697
--
-- This one needs ~ 23 seconds to run on my M1.
-- (cl) by Arno Jacobs, 2024-12-06
*/

#include <fstream>
#include <iostream>
#include <string>
#include <tuple>
#include <vector>

using namespace std;

enum Direction { North, East, South, West };

typedef tuple<int,int>      Location;   // added: -std=c++23 
typedef vector<Location>    Locations;
typedef vector<string>      Grid;
typedef vector<Direction>   Directions;

Location    location;
Locations   locations;  
Grid        grid;

Location getGridSize (Grid grid) {
    int my = grid.size();
    int mx = grid[0].size();
    return (make_tuple(mx,my));
}

Location getStartPosition (Grid grid) {
    int x, y, px, py;
    Location mxy = getGridSize(grid);

    for (y=0; y < get<1>(mxy); y++) {
        for (x=0; x < get<0>(mxy); x++) {
            if (grid[y][x] == '^') { 
                px  = x;
                py  = y;
                y   = get<1>(mxy); 
                break; 
            } 
        }
    }
    return (make_tuple(px,py));
}

Direction turnRight90Degrees (Direction direction) {
    Direction turnedDirection;
    switch (direction) {
        case North:     turnedDirection = East;     break;
        case East:      turnedDirection = South;    break;
        case South:     turnedDirection = West;     break;
        default:        turnedDirection = North;
    }
    return (turnedDirection);
}

Location oneStep (int cpx, int cpy, Direction direction) {
    int nx = cpx;
    int ny = cpy;
    switch (direction) {
        case North:     ny--;   break;
        case East:      nx++;   break;
        case South:     ny++;   break;
        default:        nx--;
    }
    return (make_tuple(nx,ny));
}

/// Part one ---------------------------------------------------------------
//

Locations walkTheWalk(Grid grid) {
    enum Direction direction    = North;   // Start direction
    Location    gridSize        = getGridSize(grid);
    Location    currentLocation = getStartPosition(grid);
    Location    nextLocation;
    Locations   path;
    
    int mx = get<0>(gridSize);
    int my = get<1>(gridSize);

    // start position is also current position
    int cx = get<0>(currentLocation);
    int cy = get<1>(currentLocation);
    int nx, ny;  // helper for next location

    while ((cx >= 0) && (cx < mx) && (cy >= 0 ) && (cy < my)) {
        currentLocation = make_tuple(cx,cy);
        path.push_back(currentLocation);

        nextLocation = oneStep(cx,cy,direction);
        nx = get<0>(nextLocation);
        ny = get<1>(nextLocation);
        
        if ((nx >= 0) && (nx < mx) && (ny >= 0 ) && (ny < my)) {
            if (grid[ny][nx] != '#') {
                cx = nx;
                cy = ny;
            } else {
                direction = turnRight90Degrees (direction);
            }
        } else {
            cx = nx;
            cy = ny;
        }
    }
    return (path);
}

Locations uniqueLocations (Locations path) {
    Locations uniques;
    for (auto p: path) {
        bool test = 0;
        for (auto u: uniques) {
            if (p == u) {
                test = 1;
                break;
            }
        }
        if (test == 0) { uniques.push_back(p); }
    }
    return (uniques);
}

int partOne (Grid grid) {
    Locations path = uniqueLocations(walkTheWalk(grid));
    return (path.size());
}


/// Part two ---------------------------------------------------------------
//

int isOneCircle(Locations path,             Directions directions, 
                Location  currentLocation,  Direction  currentDirection) {
    int oneCircle = 0;
    for (int ix=0; ix < path.size(); ix++) {
        if ((path[ix]       == currentLocation) 
        &&  (directions[ix] == currentDirection)) {
            oneCircle = 1;
        }
    }
    return (oneCircle);
} 

int walkTheCircle(Grid grid, Locations obstacles) {
    enum Direction direction;
    Location    gridSize        = getGridSize(grid);
    Location    startLocation   = getStartPosition(grid);
    Location    currentLocation;
    Location    nextLocation;
    // Check data for the circle path
    Locations   path;
    Directions  directions;
    int         circleCount = 0;
    int         oneCircle   = 0;
    int px = 0;
    int py = 0;
    
    int mx = get<0>(gridSize);
    int my = get<1>(gridSize);

    // Start with index '1', skip the start position
    for (int ix=1; ix<obstacles.size(); ix++) {

        // Clear path and directions for every new obstacle
        path.clear();
        directions.clear();
        oneCircle = 0;

        int ox = get<0>(obstacles[ix]);
        int oy = get<1>(obstacles[ix]);
        grid [oy][ox] = '#';
        grid [py][px] = '.';
        px = ox;
        py = oy;

        // start position is also current position
        currentLocation = startLocation;
        int cx = get<0>(currentLocation);
        int cy = get<1>(currentLocation);
        direction = North;      // Start direction

        int nx, ny;             // helper for next location

        while   ((oneCircle == 0) 
            &&  ((cx >= 0) && (cx < mx) && (cy >= 0 ) && (cy < my))) {

            path.push_back(currentLocation);
            directions.push_back(direction);

            nextLocation = oneStep(cx,cy,direction);
            nx = get<0>(nextLocation);
            ny = get<1>(nextLocation);
            
            if ((nx >= 0) && (nx < mx) && (ny >= 0 ) && (ny < my)) {
                if (grid[ny][nx] != '#') {      // either step 
                    cx = nx;
                    cy = ny;
                } else {                        // or turn
                    direction = turnRight90Degrees (direction);
                }
            } else {                            // Leaving the grid
                cx = nx;
                cy = ny;
            }
            currentLocation = make_tuple(cx,cy);
            oneCircle = isOneCircle(path,directions,currentLocation,direction); 
        }
        circleCount += oneCircle;
    }
    return (circleCount);
}

int partTwo (Grid grid) {
    Locations path = uniqueLocations(walkTheWalk(grid));
    return (walkTheCircle(grid,path));
}


int main() { 
    ifstream file("data/inputDay06_2024.txt");
    string lines; 

    // Read data and store in two seperate lists
    while (getline(file, lines)) { grid.push_back(lines); }
    // close file when done
    file.close();
    
    cout << "Advent of Code 2024 - day 6  (C++)" << endl;       
    cout << "Part one: Number of distinct positions:  " << partOne(grid) << endl;
// Running the next line will take ~ 23 seconds
    cout << "Part two: Number of different positions: " << partTwo(grid) << endl;
    cout << "0K." << endl << endl;

    return (0);
}

// End of code
