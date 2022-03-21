module vecfun

  use iso_fortran_env, only:i1 => int8, i2 => int16, i4 => int32, i8 => int64, &
                          r4 => real32, r8 => real64, r16 => real128

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
  public :: reverse
  public :: every

  interface push
    module procedure push_i1
    module procedure push_i2
    module procedure push_i4
    module procedure push_i8
    module procedure push_r4
    module procedure push_r8
    module procedure push_r16
    module procedure push_c4
    module procedure push_c8
    module procedure push_c16
  end interface push

  interface pushto
    module procedure pushto_i1
    module procedure pushto_i2
    module procedure pushto_i4
    module procedure pushto_i8
    module procedure pushto_r4
    module procedure pushto_r8
    module procedure pushto_r16
    module procedure pushto_c4
    module procedure pushto_c8
    module procedure pushto_c16
  end interface pushto

  interface pushnew
    module procedure pushnew_i1
    module procedure pushnew_i2
    module procedure pushnew_i4
    module procedure pushnew_i8
    module procedure pushnew_r4
    module procedure pushnew_r8
    module procedure pushnew_r16
    module procedure pushnew_c4
    module procedure pushnew_c8
    module procedure pushnew_c16
  end interface pushnew

  interface pop
    module procedure pop_i1
    module procedure pop_i2
    module procedure pop_i4
    module procedure pop_i8
    module procedure pop_r4
    module procedure pop_r8
    module procedure pop_r16
    module procedure pop_c4
    module procedure pop_c8
    module procedure pop_c16
  end interface pop

  interface popval
    module procedure popval_i1
    module procedure popval_i2
    module procedure popval_i4
    module procedure popval_i8
    module procedure popval_r4
    module procedure popval_r8
    module procedure popval_r16
    module procedure popval_c4
    module procedure popval_c8
    module procedure popval_c16
  end interface popval

  interface popall
    module procedure popall_i1
    module procedure popall_i2
    module procedure popall_i4
    module procedure popall_i8
    module procedure popall_r4
    module procedure popall_r8
    module procedure popall_r16
    module procedure popall_c4
    module procedure popall_c8
    module procedure popall_c16
  end interface popall

  interface concat
    module procedure concat_i1
    module procedure concat_i2
    module procedure concat_i4
    module procedure concat_i8
    module procedure concat_r4
    module procedure concat_r8
    module procedure concat_r16
    module procedure concat_c4
    module procedure concat_c8
    module procedure concat_c16
  end interface concat

  interface tally
    module procedure tally_i1
    module procedure tally_i2
    module procedure tally_i4
    module procedure tally_i8
    module procedure tally_r4
    module procedure tally_r8
    module procedure tally_r16
    module procedure tally_c4
    module procedure tally_c8
    module procedure tally_c16
  end interface tally

  interface echo
    module procedure echo_int
  end interface echo

  interface unique
    module procedure unique_int
  end interface unique

  interface reverse
    module procedure reverse_int
  end interface reverse

  interface every
    module procedure every_int
  end interface every

  contains

    function push_i1(vec, val) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i1), intent(in)                               :: val
      integer(kind=i1)                                           :: res
      vec = [vec, val]
      res = size(vec, kind=i1)
    end function push_i1

    function push_i2(vec, val) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i2), intent(in)                               :: val
      integer(kind=i2)                                           :: res
      vec = [vec, val]
      res = size(vec, kind=i2)
    end function push_i2

    function push_i4(vec, val) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i4), intent(in)                               :: val
      integer(kind=i4)                                           :: res
      vec = [vec, val]
      res = size(vec, kind=i4)
    end function push_i4

    function push_i8(vec, val) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i8), intent(in)                               :: val
      integer(kind=i8)                                           :: res
      vec = [vec, val]
      res = size(vec, kind=i8)
    end function push_i8

    function push_r4(vec, val) result(res)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r4), intent(in)                               :: val
      integer                                           :: res
      vec = [vec, val]
      res = size(vec)
    end function push_r4

    function push_r8(vec, val) result(res)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r8), intent(in)                               :: val
      integer                                           :: res
      vec = [vec, val]
      res = size(vec)
    end function push_r8

    function push_r16(vec, val) result(res)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r16), intent(in)                               :: val
      integer                                           :: res
      vec = [vec, val]
      res = size(vec)
    end function push_r16

    function push_c4(vec, val) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r4), intent(in)                               :: val
      integer                                           :: res
      vec = [vec, val]
      res = size(vec)
    end function push_c4

    function push_c8(vec, val) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r8), intent(in)                               :: val
      integer                                           :: res
      vec = [vec, val]
      res = size(vec)
    end function push_c8

    function push_c16(vec, val) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r16), intent(in)                               :: val
      integer                                           :: res
      vec = [vec, val]
      res = size(vec)
    end function push_c16

    function pushto_i1(vec, val, idx) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i1), intent(in)                               :: val
      integer(kind=i1), intent(in)                               :: idx
      integer(kind=i1)                                           :: res
      vec = [vec(:idx-1_i1), val, vec(idx:)]
      res = size(vec, kind=i1)
    end function pushto_i1

    function pushto_i2(vec, val, idx) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i2), intent(in)                               :: val
      integer(kind=i2), intent(in)                               :: idx
      integer(kind=i2)                                           :: res
      vec = [vec(:idx-1_i2), val, vec(idx:)]
      res = size(vec, kind=i2)
    end function pushto_i2

    function pushto_i4(vec, val, idx) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i4), intent(in)                               :: val
      integer(kind=i4), intent(in)                               :: idx
      integer(kind=i4)                                           :: res
      vec = [vec(:idx-1_i4), val, vec(idx:)]
      res = size(vec, kind=i4)
    end function pushto_i4

    function pushto_i8(vec, val, idx) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i8), intent(in)                               :: val
      integer(kind=i8), intent(in)                               :: idx
      integer(kind=i8)                                           :: res
      vec = [vec(:idx-1_i8), val, vec(idx:)]
      res = size(vec, kind=i8)
    end function pushto_i8

    function pushto_r4(vec, val, idx) result(res)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r4), intent(in)                               :: val
      integer, intent(in)                               :: idx
      integer                                           :: res
      vec = [vec(:idx-1), val, vec(idx:)]
      res = size(vec)
    end function pushto_r4

    function pushto_r8(vec, val, idx) result(res)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r8), intent(in)                               :: val
      integer, intent(in)                               :: idx
      integer                                           :: res
      vec = [vec(:idx-1), val, vec(idx:)]
      res = size(vec)
    end function pushto_r8

    function pushto_r16(vec, val, idx) result(res)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r16), intent(in)                               :: val
      integer, intent(in)                               :: idx
      integer                                           :: res
      vec = [vec(:idx-1), val, vec(idx:)]
      res = size(vec)
    end function pushto_r16

    function pushto_c4(vec, val, idx) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r4), intent(in)                               :: val
      integer, intent(in)                               :: idx
      integer                                           :: res
      vec = [vec(:idx-1), val, vec(idx:)]
      res = size(vec)
    end function pushto_c4

    function pushto_c8(vec, val, idx) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r8), intent(in)                               :: val
      integer, intent(in)                               :: idx
      integer                                           :: res
      vec = [vec(:idx-1), val, vec(idx:)]
      res = size(vec)
    end function pushto_c8

    function pushto_c16(vec, val, idx) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r16), intent(in)                               :: val
      integer, intent(in)                               :: idx
      integer                                           :: res
      vec = [vec(:idx-1), val, vec(idx:)]
      res = size(vec)
    end function pushto_c16

    function pushnew_i1(vec, val) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i1), intent(in)                               :: val
      integer(kind=i1)                                           :: res
      if (.not.any(vec.eq.val)) then
        vec = [vec, val]
      end if
      res = size(vec, kind=i1)
    end function pushnew_i1

    function pushnew_i2(vec, val) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i2), intent(in)                               :: val
      integer(kind=i2)                                           :: res
      if (.not.any(vec.eq.val)) then
        vec = [vec, val]
      end if
      res = size(vec, kind=i2)
    end function pushnew_i2

    function pushnew_i4(vec, val) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i4), intent(in)                               :: val
      integer(kind=i4)                                           :: res
      if (.not.any(vec.eq.val)) then
        vec = [vec, val]
      end if
      res = size(vec, kind=i4)
    end function pushnew_i4

    function pushnew_i8(vec, val) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i8), intent(in)                               :: val
      integer(kind=i8)                                           :: res
      if (.not.any(vec.eq.val)) then
        vec = [vec, val]
      end if
      res = size(vec, kind=i8)
    end function pushnew_i8

    function pushnew_r4(vec, val) result(res)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r4), intent(in)                               :: val
      integer                                           :: res
      if (.not.any(vec.eq.val)) then
        vec = [vec, val]
      end if
      res = size(vec)
    end function pushnew_r4

    function pushnew_r8(vec, val) result(res)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r8), intent(in)                               :: val
      integer                                           :: res
      if (.not.any(vec.eq.val)) then
        vec = [vec, val]
      end if
      res = size(vec)
    end function pushnew_r8

    function pushnew_r16(vec, val) result(res)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r16), intent(in)                               :: val
      integer                                           :: res
      if (.not.any(vec.eq.val)) then
        vec = [vec, val]
      end if
      res = size(vec)
    end function pushnew_r16

    function pushnew_c4(vec, val) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r4), intent(in)                               :: val
      integer                                           :: res
      if (.not.any(vec.eq.val)) then
        vec = [vec, val]
      end if
      res = size(vec)
    end function pushnew_c4

    function pushnew_c8(vec, val) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r8), intent(in)                               :: val
      integer                                           :: res
      if (.not.any(vec.eq.val)) then
        vec = [vec, val]
      end if
      res = size(vec)
    end function pushnew_c8

    function pushnew_c16(vec, val) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r16), intent(in)                               :: val
      integer                                           :: res
      if (.not.any(vec.eq.val)) then
        vec = [vec, val]
      end if
      res = size(vec)
    end function pushnew_c16

    function pop_i1(vec, idx) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i1), intent(in), optional                     :: idx
      integer(kind=i1)                                           :: tmp
      integer(kind=i1)                                           :: res
      if (.not.present(idx)) then
        tmp = size(vec, kind=i1)
      else
        tmp = idx
      end if
      res = vec(tmp)
      if (tmp.le.size(vec, kind=i1)) then
        vec = [vec(:tmp-1_i1), vec(tmp+1_i1:)]
      else
        vec = [vec(:tmp-1_i1)]
      end if
    end function pop_i1

    function pop_i2(vec, idx) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i2), intent(in), optional                     :: idx
      integer(kind=i2)                                           :: tmp
      integer(kind=i2)                                           :: res
      if (.not.present(idx)) then
        tmp = size(vec, kind=i2)
      else
        tmp = idx
      end if
      res = vec(tmp)
      if (tmp.le.size(vec, kind=i2)) then
        vec = [vec(:tmp-1_i2), vec(tmp+1_i2:)]
      else
        vec = [vec(:tmp-1_i2)]
      end if
    end function pop_i2

    function pop_i4(vec, idx) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i4), intent(in), optional                     :: idx
      integer(kind=i4)                                           :: tmp
      integer(kind=i4)                                           :: res
      if (.not.present(idx)) then
        tmp = size(vec, kind=i4)
      else
        tmp = idx
      end if
      res = vec(tmp)
      if (tmp.le.size(vec, kind=i4)) then
        vec = [vec(:tmp-1_i4), vec(tmp+1_i4:)]
      else
        vec = [vec(:tmp-1_i4)]
      end if
    end function pop_i4

    function pop_i8(vec, idx) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i8), intent(in), optional                     :: idx
      integer(kind=i8)                                           :: tmp
      integer(kind=i8)                                           :: res
      if (.not.present(idx)) then
        tmp = size(vec, kind=i8)
      else
        tmp = idx
      end if
      res = vec(tmp)
      if (tmp.le.size(vec, kind=i8)) then
        vec = [vec(:tmp-1_i8), vec(tmp+1_i8:)]
      else
        vec = [vec(:tmp-1_i8)]
      end if
    end function pop_i8

    function pop_r4(vec, idx) result(res)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
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
    end function pop_r4

    function pop_r8(vec, idx) result(res)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
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
    end function pop_r8

    function pop_r16(vec, idx) result(res)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
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
    end function pop_r16

    function pop_c4(vec, idx) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
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
    end function pop_c4

    function pop_c8(vec, idx) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
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
    end function pop_c8

    function pop_c16(vec, idx) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
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
    end function pop_c16

    function popval_i1(vec, val) result(idx)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i1), intent(in)                               :: val
      integer(kind=i1), dimension(1)                             :: tmp
      integer(kind=i1)                                           :: idx
      tmp = findloc(vec, val, kind=i1)
      idx = tmp(1)
      vec = [vec(:idx-1_i1), vec(idx:)]
    end function popval_i1

    function popval_i2(vec, val) result(idx)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i2), intent(in)                               :: val
      integer(kind=i2), dimension(1)                             :: tmp
      integer(kind=i2)                                           :: idx
      tmp = findloc(vec, val, kind=i2)
      idx = tmp(1)
      vec = [vec(:idx-1_i2), vec(idx:)]
    end function popval_i2

    function popval_i4(vec, val) result(idx)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i4), intent(in)                               :: val
      integer(kind=i4), dimension(1)                             :: tmp
      integer(kind=i4)                                           :: idx
      tmp = findloc(vec, val, kind=i4)
      idx = tmp(1)
      vec = [vec(:idx-1_i4), vec(idx:)]
    end function popval_i4

    function popval_i8(vec, val) result(idx)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i8), intent(in)                               :: val
      integer(kind=i8), dimension(1)                             :: tmp
      integer(kind=i8)                                           :: idx
      tmp = findloc(vec, val, kind=i8)
      idx = tmp(1)
      vec = [vec(:idx-1_i8), vec(idx:)]
    end function popval_i8

    function popval_r4(vec, val) result(idx)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r4), intent(in)                               :: val
      integer, dimension(1)                             :: tmp
      integer                                           :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end function popval_r4

    function popval_r8(vec, val) result(idx)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r8), intent(in)                               :: val
      integer, dimension(1)                             :: tmp
      integer                                           :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end function popval_r8

    function popval_r16(vec, val) result(idx)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r16), intent(in)                               :: val
      integer, dimension(1)                             :: tmp
      integer                                           :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end function popval_r16

    function popval_c4(vec, val) result(idx)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r4), intent(in)                               :: val
      integer, dimension(1)                             :: tmp
      integer                                           :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end function popval_c4

    function popval_c8(vec, val) result(idx)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r8), intent(in)                               :: val
      integer, dimension(1)                             :: tmp
      integer                                           :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end function popval_c8

    function popval_c16(vec, val) result(idx)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r16), intent(in)                               :: val
      integer, dimension(1)                             :: tmp
      integer                                           :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end function popval_c16

    function popall_i1(vec, val) result(idx)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i1), intent(in)                               :: val
      integer(kind=i1)                                           :: idx, res
      idx = 0_i1
      do while (any(vec.eq.val))
        res = popval(vec, val)
        idx = idx + 1_i1
      end do
    end function popall_i1

    function popall_i2(vec, val) result(idx)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i2), intent(in)                               :: val
      integer(kind=i2)                                           :: idx, res
      idx = 0_i2
      do while (any(vec.eq.val))
        res = popval(vec, val)
        idx = idx + 1_i2
      end do
    end function popall_i2

    function popall_i4(vec, val) result(idx)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i4), intent(in)                               :: val
      integer(kind=i4)                                           :: idx, res
      idx = 0_i4
      do while (any(vec.eq.val))
        res = popval(vec, val)
        idx = idx + 1_i4
      end do
    end function popall_i4

    function popall_i8(vec, val) result(idx)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i8), intent(in)                               :: val
      integer(kind=i8)                                           :: idx, res
      idx = 0_i8
      do while (any(vec.eq.val))
        res = popval(vec, val)
        idx = idx + 1_i8
      end do
    end function popall_i8

    function popall_r4(vec, val) result(idx)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r4), intent(in)                               :: val
      integer                                           :: idx, res
      idx = 0
      do while (any(vec.eq.val))
        res = popval(vec, val)
        idx = idx + 1
      end do
    end function popall_r4

    function popall_r8(vec, val) result(idx)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r8), intent(in)                               :: val
      integer                                           :: idx, res
      idx = 0
      do while (any(vec.eq.val))
        res = popval(vec, val)
        idx = idx + 1
      end do
    end function popall_r8

    function popall_r16(vec, val) result(idx)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r16), intent(in)                               :: val
      integer                                           :: idx, res
      idx = 0
      do while (any(vec.eq.val))
        res = popval(vec, val)
        idx = idx + 1
      end do
    end function popall_r16

    function popall_c4(vec, val) result(idx)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r4), intent(in)                               :: val
      integer                                           :: idx, res
      idx = 0
      do while (any(vec.eq.val))
        res = popval(vec, val)
        idx = idx + 1
      end do
    end function popall_c4

    function popall_c8(vec, val) result(idx)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r8), intent(in)                               :: val
      integer                                           :: idx, res
      idx = 0
      do while (any(vec.eq.val))
        res = popval(vec, val)
        idx = idx + 1
      end do
    end function popall_c8

    function popall_c16(vec, val) result(idx)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r16), intent(in)                               :: val
      integer                                           :: idx, res
      idx = 0
      do while (any(vec.eq.val))
        res = popval(vec, val)
        idx = idx + 1
      end do
    end function popall_c16

    function concat_i1(vec1, vec2) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec1
      integer(kind=i1), dimension(:), intent(in)                 :: vec2
      integer(kind=i1)                                           :: res
      vec1 = [vec1, vec2]
      res = size(vec1, kind=i1)
    end function concat_i1

    function concat_i2(vec1, vec2) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec1
      integer(kind=i2), dimension(:), intent(in)                 :: vec2
      integer(kind=i2)                                           :: res
      vec1 = [vec1, vec2]
      res = size(vec1, kind=i2)
    end function concat_i2

    function concat_i4(vec1, vec2) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec1
      integer(kind=i4), dimension(:), intent(in)                 :: vec2
      integer(kind=i4)                                           :: res
      vec1 = [vec1, vec2]
      res = size(vec1, kind=i4)
    end function concat_i4

    function concat_i8(vec1, vec2) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec1
      integer(kind=i8), dimension(:), intent(in)                 :: vec2
      integer(kind=i8)                                           :: res
      vec1 = [vec1, vec2]
      res = size(vec1, kind=i8)
    end function concat_i8

    function concat_r4(vec1, vec2) result(res)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec1
      real(kind=r4), dimension(:), intent(in)                 :: vec2
      integer                                           :: res
      vec1 = [vec1, vec2]
      res = size(vec1)
    end function concat_r4

    function concat_r8(vec1, vec2) result(res)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec1
      real(kind=r8), dimension(:), intent(in)                 :: vec2
      integer                                           :: res
      vec1 = [vec1, vec2]
      res = size(vec1)
    end function concat_r8

    function concat_r16(vec1, vec2) result(res)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec1
      real(kind=r16), dimension(:), intent(in)                 :: vec2
      integer                                           :: res
      vec1 = [vec1, vec2]
      res = size(vec1)
    end function concat_r16

    function concat_c4(vec1, vec2) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec1
      complex(kind=r4), dimension(:), intent(in)                 :: vec2
      integer                                           :: res
      vec1 = [vec1, vec2]
      res = size(vec1)
    end function concat_c4

    function concat_c8(vec1, vec2) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec1
      complex(kind=r8), dimension(:), intent(in)                 :: vec2
      integer                                           :: res
      vec1 = [vec1, vec2]
      res = size(vec1)
    end function concat_c8

    function concat_c16(vec1, vec2) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec1
      complex(kind=r16), dimension(:), intent(in)                 :: vec2
      integer                                           :: res
      vec1 = [vec1, vec2]
      res = size(vec1)
    end function concat_c16

    function tally_int(vec, val) result(res)
      integer, dimension(:), allocatable, intent(in)    :: vec
      integer, intent(in)                               :: val
      integer                                           :: i, j, k, res
      res = 0
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      do i = j, k
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
      integer                                           :: res, prev, i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        tmp = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            res = push(tmp, prev)
          end if
        end do
      else
        j = lbound(vec, 1)
        prev = vec(j)
        tmp = [prev]
        j = j + 1
        k = ubound(vec, 1)
        do i = j, k
          prev = vec(i)
          if (.not. any(tmp.eq.prev)) res = push(tmp, prev)
        end do
      end if
      vec = tmp
      res = size(vec)
    end function unique_int

    function reverse_int(vec) result(res)
      integer, dimension(:), allocatable, intent(inout) :: vec
      integer                                           :: tmp
      integer                                           :: j, k, res
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      do while (j.le.k)
        tmp = vec(j)
        vec(j) = vec(k)
        vec(k) = tmp
        j = j + 1
        k = k - 1
      end do
      res = size(vec)
    end function reverse_int

    function every_int(vec, val) result(res)
      integer, dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in)                               :: val
      integer, allocatable                              :: tmp(:)
      integer                                           :: i, j, k, res
      j = val
      k = ubound(vec, 1)
      tmp = [vec(j)]
      do i = j+val, k, val
        res = push(tmp, vec(i))
      end do
      vec = tmp
      res = size(vec)
     end function every_int

end module vecfun

