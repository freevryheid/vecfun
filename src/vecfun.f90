module vecfun

  ! use vecfun_int
  implicit none
  private

  public :: push
  public :: pushto
  public :: pushnew
  public :: pop
  public :: popval
  public :: popall
  public :: concat
  public :: tally
  public :: echo
  public :: unique

  interface push
    module procedure push_int
  end interface push

  interface pushto
    module procedure pushto_int
  end interface pushto

  interface pushnew
    module procedure pushnew_int
  end interface pushnew

  interface pop
    module procedure pop_int
  end interface pop

  interface popval
    module procedure popval_int
  end interface popval

  interface popall
    module procedure popall_int
  end interface popall

  interface concat
    module procedure concat_int
  end interface concat

  interface tally
    module procedure tally_int
  end interface tally

  interface echo
    module procedure echo_int
  end interface echo

  interface unique
    module procedure unique_int
  end interface unique

  contains

    function push_int(vec, val) result(res)
      integer, dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in)                               :: val
      integer                                           :: res
      vec = [vec, val]
      res = size(vec)
    end function push_int

    function pushto_int(vec, val, idx) result(res)
      integer, dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in)                               :: val
      integer, intent(in)                               :: idx
      integer                                           :: res
      vec = [vec(:idx-1), val, vec(idx:)]
      res = size(vec)
    end function pushto_int

    function pushnew_int(vec, val) result(res)
      integer, dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in)                               :: val
      integer                                           :: res
      if (.not.any(vec.eq.val)) then
        vec = [vec, val]
      end if
      res = size(vec)
    end function pushnew_int

    function pop_int(vec, idx) result(res)
      integer, dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in), optional                     :: idx
      integer                                           :: tmp
      integer                                           :: res
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      res = vec(tmp)
      if (tmp.le.size(vec)) then
        vec = [vec(:tmp-1), vec(tmp+1:)]
      else
        vec = [vec(:tmp-1)]
      end if
    end function pop_int

    function popval_int(vec, val) result(idx)
      integer, dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in)                               :: val
      integer, dimension(1)                             :: tmp
      integer                                           :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end function popval_int

    function popall_int(vec, val) result(idx)
      integer, dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in)                               :: val
      integer                                           :: idx, res
      idx = 0
      do while (any(vec.eq.val))
        res = popval_int(vec, val)
        idx = idx + 1
      end do
    end function popall_int

    function concat_int(vec1, vec2) result(res)
      integer, dimension(:), allocatable, intent(inout) :: vec1
      integer, dimension(:), intent(in)                 :: vec2
      integer                                           :: res
      vec1 = [vec1, vec2]
      res = size(vec1)
    end function concat_int

    function tally_int(vec, val) result(res)
      integer, dimension(:), allocatable, intent(in)    :: vec
      integer, intent(in)                               :: val
      integer                                           :: i, res
      res = 0
      do i = 1, size(vec)
        if (vec(i).eq.val) res=res+1
      end do
    end function tally_int

    function echo_int(vec, val) result(res)
      integer, dimension(:), allocatable, intent(inout) :: vec
      integer, dimension(:), allocatable                :: tmp
      integer, intent(in)                               :: val
      integer                                           :: i, res
      tmp = vec
      do i = 1, val
        vec = [vec, tmp]
      end do
      res = size(vec)
    end function echo_int

    function unique_int(vec, sorted) result(res)
      integer, dimension(:), allocatable, intent(inout) :: vec
      integer, dimension(:), allocatable                :: tmp
      logical, intent(in)                               :: sorted
      integer                                           :: res, prev, i
      if (sorted) then
        prev = vec(1)
        tmp = [prev]
        do i = 2, size(vec)
          if (vec(i).ne.prev) then
            prev = vec(i)
            res = push_int(tmp, prev)
          end if
        end do
      else
        tmp = [vec(1)]
        do i = 2, size(vec)
          if (.not. any(tmp.eq.vec(i))) res = push_int(tmp, vec(i))
        end do
      end if
      vec = tmp
      res = size(vec)
    end function unique_int

end module vecfun

