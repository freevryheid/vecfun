module tests_int

  use vecfun
  use testdrive, only: error_type, unittest_type, new_unittest, check
  implicit none
  private
  public :: collect_tests_int

  contains

    subroutine collect_tests_int(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [                                      &
          new_unittest("test for push",    test_push)    &
        , new_unittest("test for pushto",  test_pushto)  &
        , new_unittest("test for pushnew", test_pushnew) &
        , new_unittest("test for pop",     test_pop)     &
        , new_unittest("test for popval",  test_popval)  &
        , new_unittest("test for popall",  test_popval)  &
        , new_unittest("test for concat",  test_concat)  &
        , new_unittest("test for tally",   test_tally)   &
        , new_unittest("test for echo",    test_echo)    &
        , new_unittest("test for unique",  test_unique)  &
      ]
    end subroutine collect_tests_int

    subroutine test_push(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2
      integer                                    :: res, i
      vec1 = [1, 2, 3]
      vec2 = [1, 2, 3, 4]
      res = push(vec1, 4)
      call check(error, res, 4)
      if (allocated(error)) return
      do i = 1, size(vec2)
        call check(error, vec1(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_push

    subroutine test_pushto(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2
      integer                                    :: res, i
      vec1 = [1, 2, 3]
      vec2 = [4, 1, 2, 3]
      res = pushto(vec1, 4, 1)
      call check(error, res, 4)
      if (allocated(error)) return
      do i = 1, size(vec2)
        call check(error, vec1(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_pushto

    subroutine test_pushnew(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2
      integer                                    :: res, i
      vec1 = [1, 2, 3]
      vec2 = [1, 2, 3]
      res = pushnew(vec1, 2)
      call check(error, res, 3)
      if (allocated(error)) return
      do i = 1, size(vec2)
        call check(error, vec1(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_pushnew

    subroutine test_pop(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, vec3
      integer                                    :: res, i
      vec1 = [1, 2, 3, 4]
      vec2 = [1, 2, 3]
      vec3 = [2, 3]
      res = pop(vec1)
      call check(error, res, 4)
      if (allocated(error)) return
      do i = 1, size(vec2)
        call check(error, vec1(i), vec2(i))
        if (allocated(error)) return
      end do
      res = pop(vec1, 1)
      call check(error, res, 1)
      if (allocated(error)) return
      do i = 1, size(vec3)
        call check(error, vec1(i), vec3(i))
        if (allocated(error)) return
      end do
    end subroutine test_pop

    subroutine test_popval(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2
      integer                                    :: res, i
      vec1 = [1, 2, 3, 3, 3, 3]
      vec2 = [1, 2, 3, 3, 3]
      res = popval(vec1, 3)
      call check(error, res, 3)
      if (allocated(error)) return
      do i = 1, size(vec2)
        call check(error, vec1(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_popval

    subroutine test_popall(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2
      integer                                    :: res, i
      vec1 = [1, 2, 3, 3, 3, 3]
      vec2 = [1, 2]
      res = popall(vec1, 3)
      call check(error, res, 4)
      if (allocated(error)) return
      do i = 1, size(vec2)
        call check(error, vec1(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_popall

    subroutine test_concat(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, vec3
      integer                                    :: res, i
      vec1 = [1, 2, 3, 4]
      vec2 = [5, 6, 7]
      vec3 = [1, 2, 3, 4, 5, 6, 7]
      res = concat(vec1, vec2)
      call check(error, res, 7)
      if (allocated(error)) return
      do i = 1, size(vec3)
        call check(error, vec1(i), vec3(i))
        if (allocated(error)) return
      end do
    end subroutine test_concat

    subroutine test_tally(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1
      integer                                    :: res
      vec1 = [1, 2, 3, 3, 3, 3]
      res = tally(vec1, 3)
      call check(error, res, 4)
      if (allocated(error)) return
    end subroutine test_tally

    subroutine test_echo(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2
      integer                                    :: res, i
      vec1 = [1, 2, 3]
      vec2 = [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3]
      res = echo(vec1, 3)
      call check(error, res, 12)
      if (allocated(error)) return
      do i = 1, size(vec2)
        call check(error, vec1(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_echo

    subroutine test_unique(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, vec3
      integer                                    :: res, i
      vec1 = [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3]
      vec2 = [1, 2, 3]
      vec3 = [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3]
      res = unique(vec1, .false.)
      call check(error, res, 3)
      if (allocated(error)) return
      do i = 1, size(vec2)
        call check(error, vec1(i), vec2(i))
        if (allocated(error)) return
      end do
      res = unique(vec3, .true.)
      call check(error, res, 3)
      if (allocated(error)) return
      do i = 1, size(vec2)
        call check(error, vec3(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_unique

end module tests_int