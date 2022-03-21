# vecfun
fortran vector functions

a vector being defined as a 1 dim array.

so far the following are defined:

 - push: add an element to the vec
 - pushto: add element at specified index
 - pushnew: only add if element is new
 - pop: delete last element (optionally by index)
 - popval: delete a specified value from the vec
 - popall: delete all elements having specified value
 - concat: concatenate vecs
 - tally: count number of elements having specified value
 - echo: repeat existing elements specified number of times
 - unique: remove duplicates of both sorted and unsoreted vecs
 - reverse: reverse elements in vec

i plan to make these generic.

as usual:

fpm build

fpm test

