program main
  use vecfun
  implicit none
  integer, allocatable :: a(:)
  a = [1, 2, 3, 4, 5]
  print *, a
  print *, pop(a)
  print *, a
end program main
