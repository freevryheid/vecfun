program main
  use vecfun
  implicit none
  integer, allocatable :: a(:)
  a = [1, 2, 3, 4, 5]
  print *, a
  print *, every(a,6)
  print *, a
end program main
