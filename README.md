# vecfun
fortran vector functions - a vector being defined as a 1 dim array.

this library of vector functions was developed to experiment with
dynamic arrays in modern fortran. the inspiration for this was the excellent
[functional-fortran library](https://github.com/wavebitscientific/functional-fortran).
use that instead.



## functions

the following functions are defined:

### push

returns a new vector with val pushed to the end of the input vector.

```fortran
integer, dimension(:), allocatable :: vec, res
vec = [1,2,3]
res = push(vec, 4)  ! [1,2,3,4]
```

### pushto

returns a new vector with val pushed to idx of the input vector.

```fortran
integer, dimension(:), allocatable :: vec, res
vec = [1,2,3]
res = pushto(vec, 2, 1)  ! push 2 to index 1 of vec i.e. res = [2,1,2,3]
```

### pushnew

returns a new vector adding val to the input vector but only if val is not already in the input vector.

```fortran
integer, dimension(:), allocatable :: vec, res
vec = [1,2,3]
res = pushnew(vec,2)  ! 2 already exists, so not added i.e. res = [1,2,3]
res = pushnew(res,4)  ! new element i.e. res = [1,2,3,4]
```

### pop

returns a new vector by deleting the last element in the input vector, or if idx is provided, then delete the element at index idx

```fortran
integer, dimension(:), allocatable :: vec, res
vec = [1,2,3]
res = pop(vec)    ! res = [1,2]
res = pop(res,1)  ! res = [2]
```




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
