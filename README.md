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
vec = [1,2,3]
res = pushto(vec, 2, 1)  ! push 2 to index 1 of vec i.e. res = [2,1,2,3]
```

### pushnew

returns a new vector adding val to the input vector but only if val is not already in the input vector.

```fortran
vec = [1,2,3]
res = pushnew(vec,2)  ! 2 already exists, so not added i.e. res = [1,2,3]
res = pushnew(res,4)  ! new element i.e. res = [1,2,3,4]
```

### pop

returns a new vector by deleting the last element in the input vector, or if idx is provided, then delete the element at index idx

```fortran
vec = [1,2,3]
res = pop(vec)    ! res = [1,2]
res = pop(res,1)  ! res = [2]
```

### popval

returns a new vector with val deleted from the input vector. only the first val is deleted if there are more vals in the input vector.

```fortran
vec = [1,2,3,2]
res = popval(vec,2)    ! res = [1,3,2]
```

### popall

returns a new vector with all vals deleted from the input vector.

```fortran
vec = [1,2,3,2]
res = popall(vec,2)    ! res = [1,3]
```

### concat

returns a new vector joining two input vectors.

```fortran
vec1 = [1,2,3]
vec2 = [4,5,6]
res = concat(vec1,vec2)  ! res = [1,2,3,4,5,6]
```

### echo

alternative to spread, it returns a new vector by replicating the elements in the input vector val times.

```fortran
vec = [1,2,3]
res = echo(vec,2)  ! res = [1,2,3,1,2,3]
```

### unique

returns a new vector comprising the unique elements of the input vector. a faster implementation is provided for pre-sorted inputs.

```fortran
vec = [1,2,3,2]
res = unique(vec)  ! res = [1,2,3]
```

### reverse

returns a new vector reversing the elements of the input. alternative to b = a[j:k:-1]

```fortran
vec = [1,2,3]
res = reverse(vec)  ! res = [3,2,1]
```

### every

returns a new vector comprising every other consecutive val from the input vector. for example, every second element consecutively.

```fortran
vec = [1,2,3,4]
res = every(vec,2)  ! res = [2,4]
```

### zip

returns a new vector, sequentially joining two other input vectors. for example, a=[1,2]; b=[3,4]; c=zip(a,b)=[1,3,2,4]

```fortran
vec1 = [1,2,3]
vec2 = [4,5,6]
res = zip(vec1,vec2)  ! res = [1,4,2,5,3,6]
```

### popevery

returns a new vector sequentially deleting every other element from the input vector using the fortran pack function.

```fortran
vec = [1,2,3,4]
res = popevery(vec,2)  ! res = [1,3]
```

### replace

returns a new vector replacing elements in the input vector.

```fortran
vec = [1,2,3]
res = replacee(vec,2,4)  ! res = [1,4,3]
```

### swap

returns a new vector, swapping elements in the input vector.

```fortran
vec = [1,2,3]
res = swap(vec,2,3)  ! res = [1,3,2]
```

## Generics

all functions are generic and cover vecs of type:

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
 - character

some of the functions compare elements in the input vector to values.
considering the issues when comparing floats, the real and complex versions of these
functions should be considered carefully and preferably revised to include an acceptable
tolerance.

## fpm build

Add to your fpm.toml:

```
[build]
[dependencies]
vecfun.git = "https://github.com/freevryheid/vecfun"
```

## documentation

Generate the source code documentation with
[FORD](https://github.com/cmacmackin/ford). Add FORD with `pip`, for example:

```
$ python3 -m venv virtual-environment/
$ source virtual-environment/bin/activate
$ python3 -m pip install ford
```

Or, instead, just install the package in your user directory:

```
$ python3 -m pip install --user ford
```

Then, run:

```
$ ford vecfun.md
```

Open `doc/index.html` in a web browser.

## tests

fpm test currently only tests integer vectors.



