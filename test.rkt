#lang spreadsheet
// comment
set a1 2
set b2 "apple"
// another comment
set a2 a1
set a1 3.5

mode static
set a1 3
mode dynamic

set c1 2
set d1 3

// Circular reference doesn't matter because they are ordered. 
set c1 d1
set d1 c1
//mode dynamic

set a3 e4
