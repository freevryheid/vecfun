program main
  use vecfun
  implicit none
  integer, dimension(:), allocatable :: a, b
  a = [1, 2, 3, 4, 5]
  print *, a
  b = push(a, 0)
  print *, b
end program main
