# TriArrayList 
## Goals:
1. O(1) appends 
2. O(1) removals 
3. O(1) access/reads
4. stable indices

## How? 
- continas 3 arraylists: 
1. data arraylist: contains the data 
2. index arraylist: contains indices to data at that point 
3. id arraylist: contains which indices to update after a swap remove
- uses swap remove for speed
