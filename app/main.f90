program main
  use vecfun
  implicit none
  integer, allocatable :: a(:)
  a = [1, 2, 3, 4, 5]
  print *, a
  call every(a, 2)
  print *, a
end program main
