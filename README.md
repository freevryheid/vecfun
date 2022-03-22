# vecfun
fortran vector functions

a vector being defined as a 1 dim array.

so far the following are defined:

 - push: add an element to the (end of) vec
 - pushto: add element to vec at specified index
 - pushnew: only add if element is new
 - pop: delete last element or optionally by index
 - popval: delete a specified value from the vec
 - popall: delete all elements having specified value
 - concat: concatenate vecs
 - tally: count number of elements having specified value
 - echo: repeat existing elements specified number of times
 - unique: remove duplicates of both sorted and unsorted vecs
 - reverse: reverse elements in vec
 - every: returns vec comprising every other val provided

these are generic and cover vecs of type:

 - int8
 - int16
 - int32
 - int64
 - real32
 - real64
 - real128
 - complex32
 - complex64
 - complex128

fpm build

fpm test

still developing documentation ...
