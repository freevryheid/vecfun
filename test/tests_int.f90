module tests_int

  use vecfun
  use testdrive, only: error_type, unittest_type, new_unittest, check
  use stdlib_random, only : dist_rand, random_seed
  use stdlib_sorting, only: sort
  use watch_mod, only: watch, print_elapsed_times
  implicit none
  private
  public :: collect_tests_int

  contains

    subroutine collect_tests_int(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [                                        &
          new_unittest("test for push",     test_push)     &
        , new_unittest("test for pushto",   test_pushto)   &
        , new_unittest("test for pushnew",  test_pushnew)  &
        , new_unittest("test for pop",      test_pop)      &
        , new_unittest("test for popval",   test_popval)   &
        , new_unittest("test for popall",   test_popall)   &
        , new_unittest("test for concat",   test_concat)   &
        , new_unittest("test for echo",     test_echo)     &
        , new_unittest("test for unique1",  test_unique1)  &
        , new_unittest("test for unique2",  test_unique2)  &
        , new_unittest("test for reverse",  test_reverse)  &
        , new_unittest("test for every",    test_every)    &
        , new_unittest("test for zip",      test_zip)      &
        , new_unittest("test for popevery", test_popevery) &
        , new_unittest("test for replace",  test_replace)  &
        , new_unittest("test for swap",     test_swap)     &
      ]
    end subroutine collect_tests_int

    subroutine test_push(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, res
      integer                                    :: i
      vec1 = [1, 2, 3]
      vec2 = [1, 2, 3, 4]
      res = push(vec1, 4)
      do i = 1, size(vec2)
        call check(error, res(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_push

    subroutine test_pushto(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, res
      integer                                    :: i
      vec1 = [1, 2, 3]
      vec2 = [4, 1, 2, 3]
      res = pushto(vec1, 4, 1)
      do i = 1, size(vec2)
        call check(error, res(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_pushto

    subroutine test_pushnew(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, res
      integer                                    :: i
      vec1 = [1, 2, 3]
      vec2 = [1, 2, 3]
      res = pushnew(vec1, 2)
      do i = 1, size(vec2)
        call check(error, res(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_pushnew

    subroutine test_pop(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, vec3, res
      integer                                    :: i
      vec1 = [1, 2, 3, 4]
      vec2 = [1, 2, 3]
      vec3 = [2, 3]
      res = pop(vec1)
      do i = 1, size(vec2)
        call check(error, res(i), vec2(i))
        if (allocated(error)) return
      end do
      res = pop(res, 1)
      do i = 1, size(vec3)
        call check(error, res(i), vec3(i))
        if (allocated(error)) return
      end do
    end subroutine test_pop

    subroutine test_popval(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, res
      integer                                    :: i
      vec1 = [1, 2, 3, 3, 3, 3]
      vec2 = [1, 2, 3, 3, 3]
      res = popval(vec1, 3)
      do i = 1, size(vec2)
        call check(error, res(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_popval

    subroutine test_popall(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, res
      integer                                    :: i
      vec1 = [1, 2, 3, 3, 3, 3]
      vec2 = [1, 2]
      res = popall(vec1, 3)
      do i = 1, size(vec2)
        call check(error, res(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_popall

    subroutine test_concat(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, vec3, res
      integer                                    :: i
      vec1 = [1, 2, 3, 4]
      vec2 = [5, 6, 7]
      vec3 = [1, 2, 3, 4, 5, 6, 7]
      res = concat(vec1, vec2)
      do i = 1, size(vec3)
        call check(error, res(i), vec3(i))
        if (allocated(error)) return
      end do
    end subroutine test_concat

    subroutine test_echo(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, res
      integer                                    :: i
      vec1 = [1, 2, 3]
      vec2 = [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3]
      ! res = spread(vec1, 1, 3)(1)
      res = echo(vec1, 3)
      do i = 1, size(vec2)
        call check(error, res(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_echo

    subroutine test_unique1(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, vec3, res
      integer                                    :: i
      vec1 = [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3]
      vec2 = [1, 2, 3]
      vec3 = [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3]
      res = unique(vec1, .false.)
      do i = 1, size(vec2)
        call check(error, res(i), vec2(i))
        if (allocated(error)) return
      end do
      res = unique(vec3, .true.)
      do i = 1, size(vec2)
        call check(error, res(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_unique1

    subroutine test_unique2(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2
      integer                                    :: r, i
      call random_seed()
      r = dist_rand(1)
      vec1 = [r]
      do i = 2, 10000
        r = dist_rand(1)
        vec1 = push(vec1, r)
      end do
      vec2 = vec1
      call watch('init')
      call sort(vec1)
      call watch('sort vec1')
      vec1 = unique(vec1, .true.)
      call watch('unique sorted')
      vec2 = unique(vec2, .false.)
      call watch('unique unsorted')
      call sort(vec2)
      call watch('sort vec2')
      call print_elapsed_times()
      do i = 1, size(vec2)
        call check(error, vec1(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_unique2

    subroutine test_reverse(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, res
      integer                                    :: i
      vec1 = [1, 2, 3]
      vec2 = [3, 2, 1]
      res = reverse(vec1)
      do i = 1, size(vec2)
        call check(error, res(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_reverse

    subroutine test_every(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, res
      integer                                    :: i
      vec1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      vec2 = [3, 6, 9]
      res = every(vec1, 3)
      do i = 1, size(vec2)
        call check(error, res(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_every

    subroutine test_zip(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, vec3, res
      integer                                    :: i
      vec1 = [1, 2, 3]
      vec2 = [3, 6, 9]
      vec3 = [1, 3, 2, 6, 3, 9]
      res = zip(vec1, vec2)
      do i = 1, size(vec3)
        call check(error, res(i), vec3(i))
        if (allocated(error)) return
      end do
    end subroutine test_zip

    subroutine test_popevery(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, res
      integer                                    :: i
      vec1 = [1, 2, 3, 4, 5, 6]
      vec2 = [1, 3, 5]
      res = popevery(vec1, 2)
      do i = 1, size(vec2)
        call check(error, res(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_popevery

    subroutine test_replace(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, res
      integer                                    :: i
      vec1 = [1, 2, 3, 2, 5, 2]
      vec2 = [1, 3, 3, 3, 5, 3]
      res = replace(vec1, 2, 3)
      do i = 1, size(vec2)
        call check(error, res(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_replace

    subroutine test_swap(error)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), allocatable         :: vec1, vec2, res
      integer                                    :: i
      vec1 = [1, 2, 3, 4, 3, 6]
      vec2 = [3, 2, 1, 4, 1, 6]
      res = swap(vec1, 1, 3)
      do i = 1, size(vec2)
        call check(error, res(i), vec2(i))
        if (allocated(error)) return
      end do
    end subroutine test_swap

end module tests_int
