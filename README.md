# skiplist-sandbox

This was intended as a learning space to toy around with implementing my own persistent data structure in a language where there is no high-quality implementation to give me direction. Earilier in freshman year, I wrote a skiplist in C, and wanted to try in Haskell. It was a great exercise in recursion and problem solving.


## Background on the data structure
A SkipList can be seen as stacked layers of linked list, where each list element also points to its identical representation below ([see these images on wikipedia](https://en.wikipedia.org/wiki/Skip_list#Description)).
