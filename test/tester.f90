program tester
  use, intrinsic :: iso_fortran_env, only : error_unit
  use testdrive, only : run_testsuite
  use tests_int, only: collect_tests_int

  implicit none
  integer :: stat
  character(len=:), allocatable :: test

  test = "1234"

  if (scan(test, "1") > 0) then
    stat = 0
    print *, new_line('a')," ... database connection tests ... "
    call run_testsuite(collect_tests_int, error_unit, stat)
    if (stat > 0) then
      write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop
    end if
  end if

end program tester
