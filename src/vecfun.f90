! TODO address equality comparison for real and complex numbers
! use epsilon difference

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
  public :: echo
  public :: unique
  public :: reverse
  public :: every
  public :: zip
  public :: popevery
  public :: replace
  public :: swap

  interface push
    !! returns a new vector with val pushed to the end of the input vector.
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
    module procedure push_str
  end interface push

  interface pushto
    !! returns a new vector with val pushed to idx of the input vector.
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
    module procedure pushto_str
  end interface pushto

  interface pushnew
    !! returns a new vector adding val to the input vector
    !! but only if val is not already in the input vector.
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
    module procedure pushnew_str
  end interface pushnew

  interface pop
    !! returns a new vector by deleting the last element in the input vector,
    !! or if idx is provided, then delete the element at index idx
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
    module procedure pop_str
  end interface pop

  interface popval
    !! returns a new vector with val deleted from the input vector. only the
    !! first val is deleted if there are more vals in the input vector.
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
    module procedure popval_str
  end interface popval

  interface popall
    !! returns a new vector with all vals deleted from the input vector.
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
    module procedure popall_str
  end interface popall

  interface concat
    !! returns a new vector joining two input vectors.
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
    module procedure concat_str
  end interface concat

  interface echo
    !! alternative to spread, it returns a new vector by replicating
    !! the elements in the input vector val times.
    module procedure echo_i1
    module procedure echo_i2
    module procedure echo_i4
    module procedure echo_i8
    module procedure echo_r4
    module procedure echo_r8
    module procedure echo_r16
    module procedure echo_c4
    module procedure echo_c8
    module procedure echo_c16
    module procedure echo_str
  end interface echo

  interface unique
    !! returns a new vector compring the unique elements of the input vector.
    !! a faster implementation is provided for pre-sorted inputs.
    module procedure unique_i1
    module procedure unique_i2
    module procedure unique_i4
    module procedure unique_i8
    module procedure unique_r4
    module procedure unique_r8
    module procedure unique_r16
    module procedure unique_c4
    module procedure unique_c8
    module procedure unique_c16
    module procedure unique_str
  end interface unique

  interface reverse
    !! returns a new vector reversing the elements of the input.
    !! alternative to b = a[j:k:-1]
    module procedure reverse_i1
    module procedure reverse_i2
    module procedure reverse_i4
    module procedure reverse_i8
    module procedure reverse_r4
    module procedure reverse_r8
    module procedure reverse_r16
    module procedure reverse_c4
    module procedure reverse_c8
    module procedure reverse_c16
    module procedure reverse_str
  end interface reverse

  interface every
    !! returns a new vector comprising every other consecutive val from the input vector.
    !! for example, every second element consecutively.
    module procedure every_i1
    module procedure every_i2
    module procedure every_i4
    module procedure every_i8
    module procedure every_r4
    module procedure every_r8
    module procedure every_r16
    module procedure every_c4
    module procedure every_c8
    module procedure every_c16
    module procedure every_str
  end interface every

  interface zip
    !! returns a new vector, sequentially joining two other input vectors.
    !! for example, a=[1,2]; b=[3,4]; c=zip(a,b)=[1,3,2,4]
    module procedure zip_i1
    module procedure zip_i2
    module procedure zip_i4
    module procedure zip_i8
    module procedure zip_r4
    module procedure zip_r8
    module procedure zip_r16
    module procedure zip_c4
    module procedure zip_c8
    module procedure zip_c16
    module procedure zip_str
  end interface zip

  interface popevery
    !! returns a new vector sequentially deleting every other element
    !! from the input vector using the fortran pack function.
    module procedure popevery_i1
    module procedure popevery_i2
    module procedure popevery_i4
    module procedure popevery_i8
    module procedure popevery_r4
    module procedure popevery_r8
    module procedure popevery_r16
    module procedure popevery_c4
    module procedure popevery_c8
    module procedure popevery_c16
    module procedure popevery_str
  end interface popevery

  interface replace
    !! returns a new vector replacing elements in the input vector.
    module procedure replace_i1
    module procedure replace_i2
    module procedure replace_i4
    module procedure replace_i8
    module procedure replace_r4
    module procedure replace_r8
    module procedure replace_r16
    module procedure replace_c4
    module procedure replace_c8
    module procedure replace_c16
    module procedure replace_str
  end interface replace

  interface swap
    !! returns a new vector, swapping elements in the input vector.
    module procedure swap_i1
    module procedure swap_i2
    module procedure swap_i4
    module procedure swap_i8
    module procedure swap_r4
    module procedure swap_r8
    module procedure swap_r16
    module procedure swap_c4
    module procedure swap_c8
    module procedure swap_c16
    module procedure swap_str
  end interface swap

  contains

    pure function push_i1(vec, val) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i1), dimension(:), allocatable :: res
      integer(kind=i1), intent(in) :: val
      res = [vec, val]
    end function push_i1

    pure function push_i2(vec, val) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i2), dimension(:), allocatable :: res
      integer(kind=i2), intent(in) :: val
      res = [vec, val]
    end function push_i2

    pure function push_i4(vec, val) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i4), dimension(:), allocatable :: res
      integer(kind=i4), intent(in) :: val
      res = [vec, val]
    end function push_i4

    pure function push_i8(vec, val) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i8), dimension(:), allocatable :: res
      integer(kind=i8), intent(in) :: val
      res = [vec, val]
    end function push_i8

    pure function push_r4(vec, val) result(res)
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec
      real(kind=r4), dimension(:), allocatable :: res
      real(kind=r4), intent(in) :: val
      res = [vec, val]
    end function push_r4

    pure function push_r8(vec, val) result(res)
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec
      real(kind=r8), dimension(:), allocatable :: res
      real(kind=r8), intent(in) :: val
      res = [vec, val]
    end function push_r8

    pure function push_r16(vec, val) result(res)
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec
      real(kind=r16), dimension(:), allocatable :: res
      real(kind=r16), intent(in) :: val
      res = [vec, val]
    end function push_r16

    pure function push_c4(vec, val) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r4), dimension(:), allocatable :: res
      complex(kind=r4), intent(in) :: val
      res = [vec, val]
    end function push_c4

    pure function push_c8(vec, val) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r8), dimension(:), allocatable :: res
      complex(kind=r8), intent(in) :: val
      res = [vec, val]
    end function push_c8

    pure function push_c16(vec, val) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r16), dimension(:), allocatable :: res
      complex(kind=r16), intent(in) :: val
      res = [vec, val]
    end function push_c16

    pure function push_str(vec, val) result(res)
      character(:), dimension(:), allocatable, intent(in) :: vec
      character(:), dimension(:), allocatable :: res
      character(*), intent(in) :: val
      res = [vec, val]
    end function push_str

    pure function pushto_i1(vec, val, idx) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i1), dimension(:), allocatable :: res
      integer(kind=i1), intent(in) :: val
      integer, intent(in) :: idx
      res = [vec(:idx-1), val, vec(idx:)]
    end function pushto_i1

    pure function pushto_i2(vec, val, idx) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i2), dimension(:), allocatable :: res
      integer(kind=i2), intent(in) :: val
      integer, intent(in) :: idx
      res = [vec(:idx-1), val, vec(idx:)]
    end function pushto_i2

    pure function pushto_i4(vec, val, idx) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i4), dimension(:), allocatable :: res
      integer(kind=i4), intent(in) :: val
      integer, intent(in) :: idx
      res = [vec(:idx-1), val, vec(idx:)]
    end function pushto_i4

    pure function pushto_i8(vec, val, idx) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i8), dimension(:), allocatable :: res
      integer(kind=i8), intent(in) :: val
      integer, intent(in) :: idx
      res = [vec(:idx-1), val, vec(idx:)]
    end function pushto_i8

    pure function pushto_r4(vec, val, idx) result(res)
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec
      real(kind=r4), dimension(:), allocatable :: res
      real(kind=r4), intent(in) :: val
      integer, intent(in) :: idx
      res = [vec(:idx-1), val, vec(idx:)]
    end function pushto_r4

    pure function pushto_r8(vec, val, idx) result(res)
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec
      real(kind=r8), dimension(:), allocatable :: res
      real(kind=r8), intent(in) :: val
      integer, intent(in) :: idx
      res = [vec(:idx-1), val, vec(idx:)]
    end function pushto_r8

    pure function pushto_r16(vec, val, idx) result(res)
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec
      real(kind=r16), dimension(:), allocatable :: res
      real(kind=r16), intent(in) :: val
      integer, intent(in) :: idx
      res = [vec(:idx-1), val, vec(idx:)]
    end function pushto_r16

    pure function pushto_c4(vec, val, idx) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r4), dimension(:), allocatable :: res
      complex(kind=r4), intent(in) :: val
      integer, intent(in) :: idx
      res = [vec(:idx-1), val, vec(idx:)]
    end function pushto_c4

    pure function pushto_c8(vec, val, idx) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r8), dimension(:), allocatable :: res
      complex(kind=r8), intent(in) :: val
      integer, intent(in) :: idx
      res = [vec(:idx-1), val, vec(idx:)]
    end function pushto_c8

    pure function pushto_c16(vec, val, idx) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r16), dimension(:), allocatable :: res
      complex(kind=r16), intent(in) :: val
      integer, intent(in) :: idx
      res = [vec(:idx-1), val, vec(idx:)]
    end function pushto_c16

    pure function pushto_str(vec, val, idx) result(res)
      character(:), dimension(:), allocatable, intent(in) :: vec
      character(:), dimension(:), allocatable :: res
      character(*), intent(in) :: val
      integer, intent(in) :: idx
      res = [vec(:idx-1), val, vec(idx:)]
    end function pushto_str

    pure function pushnew_i1(vec, val) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i1), dimension(:), allocatable :: res
      integer(kind=i1), intent(in) :: val
      if (.not.any(vec.eq.val)) then
        res = [vec, val]
      else
        res = vec
      end if
    end function pushnew_i1

    pure function pushnew_i2(vec, val) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i2), dimension(:), allocatable :: res
      integer(kind=i2), intent(in) :: val
      if (.not.any(vec.eq.val)) then
        res = [vec, val]
      else
        res = vec
      end if
    end function pushnew_i2

    pure function pushnew_i4(vec, val) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i4), dimension(:), allocatable :: res
      integer(kind=i4), intent(in) :: val
      if (.not.any(vec.eq.val)) then
        res = [vec, val]
      else
        res = vec
      end if
    end function pushnew_i4

    pure function pushnew_i8(vec, val) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i8), dimension(:), allocatable :: res
      integer(kind=i8), intent(in) :: val
      if (.not.any(vec.eq.val)) then
        res = [vec, val]
      else
        res = vec
      end if
    end function pushnew_i8

    pure function pushnew_r4(vec, val) result(res)
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec
      real(kind=r4), dimension(:), allocatable :: res
      real(kind=r4), intent(in) :: val
      if (.not.any(vec.eq.val)) then
        res = [vec, val]
      else
        res = vec
      end if
    end function pushnew_r4

    pure function pushnew_r8(vec, val) result(res)
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec
      real(kind=r8), dimension(:), allocatable :: res
      real(kind=r8), intent(in) :: val
      if (.not.any(vec.eq.val)) then
        res = [vec, val]
      else
        res = vec
      end if
    end function pushnew_r8

    pure function pushnew_r16(vec, val) result(res)
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec
      real(kind=r16), dimension(:), allocatable :: res
      real(kind=r16), intent(in) :: val
      if (.not.any(vec.eq.val)) then
        res = [vec, val]
      else
        res = vec
      end if
    end function pushnew_r16

    pure function pushnew_c4(vec, val) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r4), dimension(:), allocatable :: res
      complex(kind=r4), intent(in) :: val
      if (.not.any(vec.eq.val)) then
        res = [vec, val]
      else
        res = vec
      end if
    end function pushnew_c4

    pure function pushnew_c8(vec, val) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r8), dimension(:), allocatable :: res
      complex(kind=r8), intent(in) :: val
      if (.not.any(vec.eq.val)) then
        res = [vec, val]
      else
        res = vec
      end if
    end function pushnew_c8

    pure function pushnew_c16(vec, val) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r16), dimension(:), allocatable :: res
      complex(kind=r16), intent(in) :: val
      if (.not.any(vec.eq.val)) then
        res = [vec, val]
      else
        res = vec
      end if
    end function pushnew_c16

    pure function pushnew_str(vec, val) result(res)
      character(:), dimension(:), allocatable, intent(in) :: vec
      character(:), dimension(:), allocatable :: res
      character(*), intent(in) :: val
      if (.not.any(vec.eq.val)) then
        res = [vec, val]
      else
        res = vec
      end if
    end function pushnew_str

    pure function pop_i1(vec, idx) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i1), dimension(:), allocatable :: res
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        res = [vec(:tmp-1), vec(tmp+1:)]
      else
        res = [vec(:tmp-1)]
      end if
    end function pop_i1

    pure function pop_i2(vec, idx) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i2), dimension(:), allocatable :: res
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        res = [vec(:tmp-1), vec(tmp+1:)]
      else
        res = [vec(:tmp-1)]
      end if
    end function pop_i2

    pure function pop_i4(vec, idx) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i4), dimension(:), allocatable :: res
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        res = [vec(:tmp-1), vec(tmp+1:)]
      else
        res = [vec(:tmp-1)]
      end if
    end function pop_i4

    pure function pop_i8(vec, idx) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i8), dimension(:), allocatable :: res
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        res = [vec(:tmp-1), vec(tmp+1:)]
      else
        res = [vec(:tmp-1)]
      end if
    end function pop_i8

    pure function pop_r4(vec, idx) result(res)
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec
      real(kind=r4), dimension(:), allocatable :: res
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        res = [vec(:tmp-1), vec(tmp+1:)]
      else
        res = [vec(:tmp-1)]
      end if
    end function pop_r4

    pure function pop_r8(vec, idx) result(res)
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec
      real(kind=r8), dimension(:), allocatable :: res
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        res = [vec(:tmp-1), vec(tmp+1:)]
      else
        res = [vec(:tmp-1)]
      end if
    end function pop_r8

    pure function pop_r16(vec, idx) result(res)
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec
      real(kind=r16), dimension(:), allocatable :: res
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        res = [vec(:tmp-1), vec(tmp+1:)]
      else
        res = [vec(:tmp-1)]
      end if
    end function pop_r16

    pure function pop_c4(vec, idx) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r4), dimension(:), allocatable :: res
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        res = [vec(:tmp-1), vec(tmp+1:)]
      else
        res = [vec(:tmp-1)]
      end if
    end function pop_c4

    pure function pop_c8(vec, idx) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r8), dimension(:), allocatable :: res
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        res = [vec(:tmp-1), vec(tmp+1:)]
      else
        res = [vec(:tmp-1)]
      end if
    end function pop_c8

    pure function pop_c16(vec, idx) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r16), dimension(:), allocatable :: res
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        res = [vec(:tmp-1), vec(tmp+1:)]
      else
        res = [vec(:tmp-1)]
      end if
    end function pop_c16

    pure function pop_str(vec, idx) result(res)
      character(:), dimension(:), allocatable, intent(in) :: vec
      character(:), dimension(:), allocatable :: res
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        res = [vec(:tmp-1), vec(tmp+1:)]
      else
        res = [vec(:tmp-1)]
      end if
    end function pop_str

    pure function popval_i1(vec, val) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i1), dimension(:), allocatable :: res
      integer(kind=i1), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      res = [vec(:idx-1), vec(idx:)]
    end function popval_i1

    pure function popval_i2(vec, val) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i2), dimension(:), allocatable :: res
      integer(kind=i2), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      res = [vec(:idx-1), vec(idx:)]
    end function popval_i2

    pure function popval_i4(vec, val) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i4), dimension(:), allocatable :: res
      integer(kind=i4), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      res = [vec(:idx-1), vec(idx:)]
    end function popval_i4

    pure function popval_i8(vec, val) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i8), dimension(:), allocatable :: res
      integer(kind=i8), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      res = [vec(:idx-1), vec(idx:)]
    end function popval_i8

    pure function popval_r4(vec, val) result(res)
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec
      real(kind=r4), dimension(:), allocatable :: res
      real(kind=r4), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val, 1)
      idx = tmp(1)
      res = [vec(:idx-1), vec(idx:)]
    end function popval_r4

    pure function popval_r8(vec, val) result(res)
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec
      real(kind=r8), dimension(:), allocatable :: res
      real(kind=r8), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      res = [vec(:idx-1), vec(idx:)]
    end function popval_r8

    pure function popval_r16(vec, val) result(res)
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec
      real(kind=r16), dimension(:), allocatable :: res
      real(kind=r16), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      res = [vec(:idx-1), vec(idx:)]
    end function popval_r16

    pure function popval_c4(vec, val) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r4), dimension(:), allocatable :: res
      complex(kind=r4), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      res = [vec(:idx-1), vec(idx:)]
    end function popval_c4

    pure function popval_c8(vec, val) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r8), dimension(:), allocatable :: res
      complex(kind=r8), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      res = [vec(:idx-1), vec(idx:)]
    end function popval_c8

    pure function popval_c16(vec, val) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r16), dimension(:), allocatable :: res
      complex(kind=r16), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      res = [vec(:idx-1), vec(idx:)]
    end function popval_c16

    pure function popval_str(vec, val) result(res)
      character(:), dimension(:), allocatable, intent(in) :: vec
      character(:), dimension(:), allocatable :: res
      character(*), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      res = [vec(:idx-1), vec(idx:)]
    end function popval_str

    ! pure function popall_i1(vec, val) result(res)
    !   integer(kind=i1), dimension(:), allocatable, intent(in) :: vec
    !   integer(kind=i1), dimension(:), allocatable :: res
    !   integer(kind=i1), intent(in) :: val
    !   logical, dimension(:), allocatable :: mask
    !   integer :: i, n
    !   mask = vec.eq.val
    !   n = count(mask)
    !   res = vec
    !   do  i = 1, n
    !     res = popval(res, val)
    !   end do
    ! end function popall_i1

    pure function popall_i1(vec, val) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i1), dimension(:), allocatable :: res
      integer(kind=i1), intent(in) :: val
      res = pack(vec, vec /= val)
    end function popall_i1

    pure function popall_i2(vec, val) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i2), dimension(:), allocatable :: res
      integer(kind=i2), intent(in) :: val
      res = pack(vec, vec /= val)
    end function popall_i2

    pure function popall_i4(vec, val) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i4), dimension(:), allocatable :: res
      integer(kind=i4), intent(in) :: val
      res = pack(vec, vec /= val)
    end function popall_i4

    pure function popall_i8(vec, val) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i8), dimension(:), allocatable :: res
      integer(kind=i8), intent(in) :: val
      res = pack(vec, vec /= val)
    end function popall_i8

    pure function popall_r4(vec, val) result(res)
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec
      real(kind=r4), dimension(:), allocatable :: res
      real(kind=r4), intent(in) :: val
      res = pack(vec, vec /= val)
    end function popall_r4

    pure function popall_r8(vec, val) result(res)
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec
      real(kind=r8), dimension(:), allocatable :: res
      real(kind=r8), intent(in) :: val
      res = pack(vec, vec /= val)
    end function popall_r8

    pure function popall_r16(vec, val) result(res)
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec
      real(kind=r16), dimension(:), allocatable :: res
      real(kind=r16), intent(in) :: val
      res = pack(vec, vec /= val)
    end function popall_r16

    pure function popall_c4(vec, val) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r4), dimension(:), allocatable :: res
      complex(kind=r4), intent(in) :: val
      res = pack(vec, vec /= val)
    end function popall_c4

    pure function popall_c8(vec, val) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r8), dimension(:), allocatable :: res
      complex(kind=r8), intent(in) :: val
      res = pack(vec, vec /= val)
    end function popall_c8

    pure function popall_c16(vec, val) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r16), dimension(:), allocatable :: res
      complex(kind=r16), intent(in) :: val
      res = pack(vec, vec /= val)
    end function popall_c16

    pure function popall_str(vec, val) result(res)
      character(:), dimension(:), allocatable, intent(in) :: vec
      character(:), dimension(:), allocatable :: res
      character(*), intent(in) :: val
      res = pack(vec, vec /= val)
    end function popall_str

    pure function concat_i1(vec1, vec2) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec1
      integer(kind=i1), dimension(:), allocatable :: res
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec2
      res = [vec1, vec2]
    end function concat_i1

    pure function concat_i2(vec1, vec2) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec1
      integer(kind=i2), dimension(:), allocatable :: res
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec2
      res = [vec1, vec2]
    end function concat_i2

    pure function concat_i4(vec1, vec2) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec1
      integer(kind=i4), dimension(:), allocatable :: res
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec2
      res = [vec1, vec2]
    end function concat_i4

    pure function concat_i8(vec1, vec2) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec1
      integer(kind=i8), dimension(:), allocatable :: res
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec2
      res = [vec1, vec2]
    end function concat_i8

    pure function concat_r4(vec1, vec2) result(res)
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec1
      real(kind=r4), dimension(:), allocatable :: res
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec2
      res = [vec1, vec2]
    end function concat_r4

    pure function concat_r8(vec1, vec2) result(res)
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec1
      real(kind=r8), dimension(:), allocatable :: res
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec2
      res = [vec1, vec2]
    end function concat_r8

    pure function concat_r16(vec1, vec2) result(res)
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec1
      real(kind=r16), dimension(:), allocatable :: res
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec2
      res = [vec1, vec2]
    end function concat_r16

    pure function concat_c4(vec1, vec2) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec1
      complex(kind=r4), dimension(:), allocatable :: res
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec2
      res = [vec1, vec2]
    end function concat_c4

    pure function concat_c8(vec1, vec2) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec1
      complex(kind=r8), dimension(:), allocatable :: res
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec2
      res = [vec1, vec2]
    end function concat_c8

    pure function concat_c16(vec1, vec2) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec1
      complex(kind=r16), dimension(:), allocatable :: res
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec2
      res = [vec1, vec2]
    end function concat_c16

    pure function concat_str(vec1, vec2) result(res)
      character(:), dimension(:), allocatable, intent(in) :: vec1
      character(:), dimension(:), allocatable :: res
      character(:), dimension(:), allocatable, intent(in) :: vec2
      res = [vec1, vec2]
    end function concat_str

    pure function echo_i1(vec, val) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i1), dimension(:), allocatable :: res
      integer(kind=i1), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      res = vec
      do i = 1, val
        res = [res, tmp]
      end do
    end function echo_i1

    pure function echo_i2(vec, val) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i2), dimension(:), allocatable :: res
      integer(kind=i2), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      res = vec
      do i = 1, val
        res = [res, tmp]
      end do
    end function echo_i2

    pure function echo_i4(vec, val) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i4), dimension(:), allocatable :: res
      integer(kind=i4), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      res = vec
      do i = 1, val
        res = [res, tmp]
      end do
    end function echo_i4

    pure function echo_i8(vec, val) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i8), dimension(:), allocatable :: res
      integer(kind=i8), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      res = vec
      do i = 1, val
        res = [res, tmp]
      end do
    end function echo_i8

    pure function echo_r4(vec, val) result(res)
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec
      real(kind=r4), dimension(:), allocatable :: res
      real(kind=r4), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      res = vec
      do i = 1, val
        res = [res, tmp]
      end do
    end function echo_r4

    pure function echo_r8(vec, val) result(res)
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec
      real(kind=r8), dimension(:), allocatable :: res
      real(kind=r8), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      res = vec
      do i = 1, val
        res = [res, tmp]
      end do
    end function echo_r8

    pure function echo_r16(vec, val) result(res)
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec
      real(kind=r16), dimension(:), allocatable :: res
      real(kind=r16), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      res = vec
      do i = 1, val
        res = [res, tmp]
      end do
    end function echo_r16

    pure function echo_c4(vec, val) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r4), dimension(:), allocatable :: res
      complex(kind=r4), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      res = vec
      do i = 1, val
        res = [res, tmp]
      end do
    end function echo_c4

    pure function echo_c8(vec, val) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r8), dimension(:), allocatable :: res
      complex(kind=r8), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      res = vec
      do i = 1, val
        res = [res, tmp]
      end do
    end function echo_c8

    pure function echo_c16(vec, val) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r16), dimension(:), allocatable :: res
      complex(kind=r16), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      res = vec
      do i = 1, val
        res = [res, tmp]
      end do
    end function echo_c16

    pure function echo_str(vec, val) result(res)
      character(:), dimension(:), allocatable, intent(in) :: vec
      character(:), dimension(:), allocatable :: res
      character(:), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      res = vec
      do i = 1, val
        res = [res, tmp]
      end do
    end function echo_str

    pure function unique_i1(vec, sorted) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i1), dimension(:), allocatable :: res
      logical, intent(in) :: sorted
      integer(kind=i1) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            res = push(res, prev)
          end if
        end do
      else
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = j + 1
        k = ubound(vec, 1)
        do i = j, k
          prev = vec(i)
          if (.not. any(res.eq.prev)) res = push(res, prev)
        end do
      end if
    end function unique_i1

    pure function unique_i2(vec, sorted) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i2), dimension(:), allocatable :: res
      logical, intent(in) :: sorted
      integer(kind=i2) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            res = push(res, prev)
          end if
        end do
      else
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = j + 1
        k = ubound(vec, 1)
        do i = j, k
          prev = vec(i)
          if (.not. any(res.eq.prev)) res = push(res, prev)
        end do
      end if
    end function unique_i2

    pure function unique_i4(vec, sorted) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i4), dimension(:), allocatable :: res
      logical, intent(in) :: sorted
      integer(kind=i4) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            res = push(res, prev)
          end if
        end do
      else
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = j + 1
        k = ubound(vec, 1)
        do i = j, k
          prev = vec(i)
          if (.not. any(res.eq.prev)) res = push(res, prev)
        end do
      end if
    end function unique_i4

    pure function unique_i8(vec, sorted) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i8), dimension(:), allocatable :: res
      logical, intent(in) :: sorted
      integer(kind=i8) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            res = push(res, prev)
          end if
        end do
      else
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = j + 1
        k = ubound(vec, 1)
        do i = j, k
          prev = vec(i)
          if (.not. any(res.eq.prev)) res = push(res, prev)
        end do
      end if
    end function unique_i8

    pure function unique_r4(vec, sorted) result(res)
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec
      real(kind=r4), dimension(:), allocatable :: res
      logical, intent(in) :: sorted
      real(kind=r4) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            res = push(res, prev)
          end if
        end do
      else
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = j + 1
        k = ubound(vec, 1)
        do i = j, k
          prev = vec(i)
          if (.not. any(res.eq.prev)) res = push(res, prev)
        end do
      end if
    end function unique_r4

    pure function unique_r8(vec, sorted) result(res)
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec
      real(kind=r8), dimension(:), allocatable :: res
      logical, intent(in) :: sorted
      real(kind=r8) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            res = push(res, prev)
          end if
        end do
      else
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = j + 1
        k = ubound(vec, 1)
        do i = j, k
          prev = vec(i)
          if (.not. any(res.eq.prev)) res = push(res, prev)
        end do
      end if
    end function unique_r8

    pure function unique_r16(vec, sorted) result(res)
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec
      real(kind=r16), dimension(:), allocatable :: res
      logical, intent(in) :: sorted
      real(kind=r16) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            res = push(res, prev)
          end if
        end do
      else
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = j + 1
        k = ubound(vec, 1)
        do i = j, k
          prev = vec(i)
          if (.not. any(res.eq.prev)) res = push(res, prev)
        end do
      end if
    end function unique_r16

    pure function unique_c4(vec, sorted) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r4), dimension(:), allocatable :: res
      logical, intent(in) :: sorted
      complex(kind=r4) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            res = push(res, prev)
          end if
        end do
      else
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = j + 1
        k = ubound(vec, 1)
        do i = j, k
          prev = vec(i)
          if (.not. any(res.eq.prev)) res = push(res, prev)
        end do
      end if
    end function unique_c4

    pure function unique_c8(vec, sorted) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r8), dimension(:), allocatable :: res
      logical, intent(in) :: sorted
      complex(kind=r8) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            res = push(res, prev)
          end if
        end do
      else
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = j + 1
        k = ubound(vec, 1)
        do i = j, k
          prev = vec(i)
          if (.not. any(res.eq.prev)) res = push(res, prev)
        end do
      end if
    end function unique_c8

    pure function unique_c16(vec, sorted) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r16), dimension(:), allocatable :: res
      logical, intent(in) :: sorted
      complex(kind=r16) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            res = push(res, prev)
          end if
        end do
      else
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = j + 1
        k = ubound(vec, 1)
        do i = j, k
          prev = vec(i)
          if (.not. any(res.eq.prev)) res = push(res, prev)
        end do
      end if
    end function unique_c16

    pure function unique_str(vec, sorted) result(res)
      character(:), dimension(:), allocatable, intent(in) :: vec
      character(:), dimension(:), allocatable :: res
      logical, intent(in) :: sorted
      character(:), allocatable :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            res = push(res, prev)
          end if
        end do
      else
        j = lbound(vec, 1)
        prev = vec(j)
        res = [prev]
        j = j + 1
        k = ubound(vec, 1)
        do i = j, k
          prev = vec(i)
          if (.not. any(res.eq.prev)) res = push(res, prev)
        end do
      end if
    end function unique_str

    pure function reverse_i1(vec) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i1), dimension(:), allocatable :: res
      res = vec(ubound(vec, 1):1:-1)
    end function reverse_i1

    pure function reverse_i2(vec) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i2), dimension(:), allocatable :: res
      res = vec(ubound(vec, 1):1:-1)
    end function reverse_i2

    pure function reverse_i4(vec) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i4), dimension(:), allocatable :: res
      res = vec(ubound(vec, 1):1:-1)
    end function reverse_i4

    pure function reverse_i8(vec) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i8), dimension(:), allocatable :: res
      res = vec(ubound(vec, 1):1:-1)
    end function reverse_i8

    pure function reverse_r4(vec) result(res)
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec
      real(kind=r4), dimension(:), allocatable :: res
      res = vec(ubound(vec, 1):1:-1)
    end function reverse_r4

    pure function reverse_r8(vec) result(res)
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec
      real(kind=r8), dimension(:), allocatable :: res
      res = vec(ubound(vec, 1):1:-1)
    end function reverse_r8

    pure function reverse_r16(vec) result(res)
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec
      real(kind=r16), dimension(:), allocatable :: res
      res = vec(ubound(vec, 1):1:-1)
    end function reverse_r16

    pure function reverse_c4(vec) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r4), dimension(:), allocatable :: res
      res = vec(ubound(vec, 1):1:-1)
    end function reverse_c4

    pure function reverse_c8(vec) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r8), dimension(:), allocatable :: res
      res = vec(ubound(vec, 1):1:-1)
    end function reverse_c8

    pure function reverse_c16(vec) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r16), dimension(:), allocatable :: res
      res = vec(ubound(vec, 1):1:-1)
    end function reverse_c16

    pure function reverse_str(vec) result(res)
      character(:), dimension(:), allocatable, intent(in) :: vec
      character(:), dimension(:), allocatable :: res
      res = vec(ubound(vec, 1):1:-1)
    end function reverse_str

    pure function every_i1(vec, val) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i1), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      res = [vec(j)]
      do i = j+val, k, val
        res = push(res, vec(i))
      end do
     end function every_i1

    pure function every_i2(vec, val) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i2), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      res = [vec(j)]
      do i = j+val, k, val
        res = push(res, vec(i))
      end do
     end function every_i2

    pure function every_i4(vec, val) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i4), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      res = [vec(j)]
      do i = j+val, k, val
        res = push(res, vec(i))
      end do
     end function every_i4

    pure function every_i8(vec, val) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i8), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      res = [vec(j)]
      do i = j+val, k, val
        res = push(res, vec(i))
      end do
     end function every_i8

    pure function every_r4(vec, val) result(res)
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec
      real(kind=r4), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      res = [vec(j)]
      do i = j+val, k, val
        res = push(res, vec(i))
      end do
     end function every_r4

    pure function every_r8(vec, val) result(res)
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec
      real(kind=r8), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      res = [vec(j)]
      do i = j+val, k, val
        res = push(res, vec(i))
      end do
     end function every_r8

    pure function every_r16(vec, val) result(res)
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec
      real(kind=r16), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      res = [vec(j)]
      do i = j+val, k, val
        res = push(res, vec(i))
      end do
     end function every_r16

    pure function every_c4(vec, val) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r4), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      res = [vec(j)]
      do i = j+val, k, val
        res = push(res, vec(i))
      end do
     end function every_c4

    pure function every_c8(vec, val) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r8), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      res = [vec(j)]
      do i = j+val, k, val
        res = push(res, vec(i))
      end do
     end function every_c8

    pure function every_c16(vec, val) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r16), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      res = [vec(j)]
      do i = j+val, k, val
        res = push(res, vec(i))
      end do
    end function every_c16

    pure function every_str(vec, val) result(res)
      character(:), dimension(:), allocatable, intent(in) :: vec
      character(:), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      res = [vec(j)]
      do i = j+val, k, val
        res = push(res, vec(i))
      end do
    end function every_str

    pure function zip_i1(vec1, vec2) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec1
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec2
      integer(kind=i1), dimension(:), allocatable :: res
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      res = vec1
      do i = j, k
        res = pushto(res, vec2(i), 2*i)
      end do
    end function zip_i1

    pure function zip_i2(vec1, vec2) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec1
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec2
      integer(kind=i2), dimension(:), allocatable :: res
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      res = vec1
      do i = j, k
        res = pushto(res, vec2(i), 2*i)
      end do
    end function zip_i2

    pure function zip_i4(vec1, vec2) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec1
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec2
      integer(kind=i4), dimension(:), allocatable :: res
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      res = vec1
      do i = j, k
        res = pushto(res, vec2(i), 2*i)
      end do
    end function zip_i4

    pure function zip_i8(vec1, vec2) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec1
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec2
      integer(kind=i8), dimension(:), allocatable :: res
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      res = vec1
      do i = j, k
        res = pushto(res, vec2(i), 2*i)
      end do
    end function zip_i8

    pure function zip_r4(vec1, vec2) result(res)
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec1
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec2
      real(kind=r4), dimension(:), allocatable :: res
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      res = vec1
      do i = j, k
        res = pushto(res, vec2(i), 2*i)
      end do
    end function zip_r4

    pure function zip_r8(vec1, vec2) result(res)
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec1
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec2
      real(kind=r8), dimension(:), allocatable :: res
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      res = vec1
      do i = j, k
        res = pushto(res, vec2(i), 2*i)
      end do
    end function zip_r8

    pure function zip_r16(vec1, vec2) result(res)
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec1
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec2
      real(kind=r16), dimension(:), allocatable :: res
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      res = vec1
      do i = j, k
        res = pushto(res, vec2(i), 2*i)
      end do
    end function zip_r16

    pure function zip_c4(vec1, vec2) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec1
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec2
      complex(kind=r4), dimension(:), allocatable :: res
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      res = vec1
      do i = j, k
        res = pushto(res, vec2(i), 2*i)
      end do
    end function zip_c4

    pure function zip_c8(vec1, vec2) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec1
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec2
      complex(kind=r8), dimension(:), allocatable :: res
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      res = vec1
      do i = j, k
        res = pushto(res, vec2(i), 2*i)
      end do
    end function zip_c8

    pure function zip_c16(vec1, vec2) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec1
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec2
      complex(kind=r16), dimension(:), allocatable :: res
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      res = vec1
      do i = j, k
        res = pushto(res, vec2(i), 2*i)
      end do
    end function zip_c16

    pure function zip_str(vec1, vec2) result(res)
      character(:), dimension(:), allocatable, intent(in) :: vec1
      character(:), dimension(:), allocatable, intent(in) :: vec2
      character(:), dimension(:), allocatable :: res
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      res = vec1
      do i = j, k
        res = pushto(res, vec2(i), 2*i)
      end do
    end function zip_str

    function popevery_i1(vec, val) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i1), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      logical, dimension(:), allocatable :: mask
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      allocate(mask(size(vec)))
      mask = .false.
      mask(j:k:val) = .true.
      res = pack(vec, mask)
      deallocate(mask)
    end function popevery_i1

    function popevery_i2(vec, val) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i2), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      logical, dimension(:), allocatable :: mask
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      allocate(mask(size(vec)))
      mask = .false.
      mask(j:k:val) = .true.
      res = pack(vec, mask)
      deallocate(mask)
    end function popevery_i2

    function popevery_i4(vec, val) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i4), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      logical, dimension(:), allocatable :: mask
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      allocate(mask(size(vec)))
      mask = .false.
      mask(j:k:val) = .true.
      res = pack(vec, mask)
      deallocate(mask)
    end function popevery_i4

    function popevery_i8(vec, val) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i8), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      logical, dimension(:), allocatable :: mask
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      allocate(mask(size(vec)))
      mask = .false.
      mask(j:k:val) = .true.
      res = pack(vec, mask)
      deallocate(mask)
    end function popevery_i8

    function popevery_r4(vec, val) result(res)
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec
      real(kind=r4), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      logical, dimension(:), allocatable :: mask
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      allocate(mask(size(vec)))
      mask = .false.
      mask(j:k:val) = .true.
      res = pack(vec, mask)
      deallocate(mask)
    end function popevery_r4

    function popevery_r8(vec, val) result(res)
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec
      real(kind=r8), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      logical, dimension(:), allocatable :: mask
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      allocate(mask(size(vec)))
      mask = .false.
      mask(j:k:val) = .true.
      res = pack(vec, mask)
      deallocate(mask)
    end function popevery_r8

    function popevery_r16(vec, val) result(res)
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec
      real(kind=r16), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      logical, dimension(:), allocatable :: mask
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      allocate(mask(size(vec)))
      mask = .false.
      mask(j:k:val) = .true.
      res = pack(vec, mask)
      deallocate(mask)
    end function popevery_r16

    function popevery_c4(vec, val) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r4), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      logical, dimension(:), allocatable :: mask
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      allocate(mask(size(vec)))
      mask = .false.
      mask(j:k:val) = .true.
      res = pack(vec, mask)
      deallocate(mask)
    end function popevery_c4

    function popevery_c8(vec, val) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r8), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      logical, dimension(:), allocatable :: mask
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      allocate(mask(size(vec)))
      mask = .false.
      mask(j:k:val) = .true.
      res = pack(vec, mask)
      deallocate(mask)
    end function popevery_c8

    function popevery_c16(vec, val) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r16), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      logical, dimension(:), allocatable :: mask
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      allocate(mask(size(vec)))
      mask = .false.
      mask(j:k:val) = .true.
      res = pack(vec, mask)
      deallocate(mask)
    end function popevery_c16

    function popevery_str(vec, val) result(res)
      character(:), dimension(:), allocatable, intent(in) :: vec
      character(:), dimension(:), allocatable :: res
      integer, intent(in) :: val
      integer :: i, j, k
      logical, dimension(:), allocatable :: mask
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      allocate(mask(size(vec)))
      mask = .false.
      mask(j:k:val) = .true.
      res = pack(vec, mask)
      deallocate(mask)
    end function popevery_str

    function replace_i1(vec, val1, val2) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i1), dimension(:), allocatable :: res
      integer(kind=i1) :: val1, val2
      res = vec
      where(vec.eq.val1) res = val2
    end function replace_i1

    function replace_i2(vec, val1, val2) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i2), dimension(:), allocatable :: res
      integer(kind=i2) :: val1, val2
      res = vec
      where(vec.eq.val1) res = val2
    end function replace_i2

    function replace_i4(vec, val1, val2) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i4), dimension(:), allocatable :: res
      integer(kind=i4) :: val1, val2
      res = vec
      where(vec.eq.val1) res = val2
    end function replace_i4

    function replace_i8(vec, val1, val2) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i8), dimension(:), allocatable :: res
      integer(kind=i8) :: val1, val2
      res = vec
      where(vec.eq.val1) res = val2
    end function replace_i8

    function replace_r4(vec, val1, val2) result(res)
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec
      real(kind=r4), dimension(:), allocatable :: res
      real(kind=r4) :: val1, val2
      res = vec
      where(vec.eq.val1) res = val2
    end function replace_r4

    function replace_r8(vec, val1, val2) result(res)
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec
      real(kind=r8), dimension(:), allocatable :: res
      real(kind=r8) :: val1, val2
      res = vec
      where(vec.eq.val1) res = val2
    end function replace_r8

    function replace_r16(vec, val1, val2) result(res)
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec
      real(kind=r16), dimension(:), allocatable :: res
      real(kind=r16) :: val1, val2
      res = vec
      where(vec.eq.val1) res = val2
    end function replace_r16

    function replace_c4(vec, val1, val2) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r4), dimension(:), allocatable :: res
      complex(kind=r4) :: val1, val2
      res = vec
      where(vec.eq.val1) res = val2
    end function replace_c4

    function replace_c8(vec, val1, val2) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r8), dimension(:), allocatable :: res
      complex(kind=r8) :: val1, val2
      res = vec
      where(vec.eq.val1) res = val2
    end function replace_c8

    function replace_c16(vec, val1, val2) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r16), dimension(:), allocatable :: res
      complex(kind=r16) :: val1, val2
      res = vec
      where(vec.eq.val1) res = val2
    end function replace_c16

    function replace_str(vec, val1, val2) result(res)
      character(:), dimension(:), allocatable, intent(in) :: vec
      character(:), dimension(:), allocatable :: res
      character(*) :: val1, val2
      res = vec
      where(vec.eq.val1) res = val2
    end function replace_str

    function swap_i1(vec, val1, val2) result(res)
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i1), dimension(:), allocatable :: res
      integer(kind=i1) :: val1, val2
      logical, dimension(:), allocatable :: mask1, mask2
      allocate(mask1(size(vec)))
      allocate(mask2(size(vec)))
      mask1 = .false.
      mask2 = .false.
      where(vec.eq.val1) mask1 = .true.
      where(vec.eq.val2) mask2 = .true.
      res = vec
      where(mask1) res = val2
      where(mask2) res = val1
      deallocate(mask1)
      deallocate(mask2)
    end function swap_i1

    function swap_i2(vec, val1, val2) result(res)
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i2), dimension(:), allocatable :: res
      integer(kind=i2) :: val1, val2
      logical, dimension(:), allocatable :: mask1, mask2
      allocate(mask1(size(vec)))
      allocate(mask2(size(vec)))
      mask1 = .false.
      mask2 = .false.
      where(vec.eq.val1) mask1 = .true.
      where(vec.eq.val2) mask2 = .true.
      res = vec
      where(mask1) res = val2
      where(mask2) res = val1
      deallocate(mask1)
      deallocate(mask2)
    end function swap_i2

    function swap_i4(vec, val1, val2) result(res)
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i4), dimension(:), allocatable :: res
      integer(kind=i4) :: val1, val2
      logical, dimension(:), allocatable :: mask1, mask2
      allocate(mask1(size(vec)))
      allocate(mask2(size(vec)))
      mask1 = .false.
      mask2 = .false.
      where(vec.eq.val1) mask1 = .true.
      where(vec.eq.val2) mask2 = .true.
      res = vec
      where(mask1) res = val2
      where(mask2) res = val1
      deallocate(mask1)
      deallocate(mask2)
    end function swap_i4

    function swap_i8(vec, val1, val2) result(res)
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec
      integer(kind=i8), dimension(:), allocatable :: res
      integer(kind=i8) :: val1, val2
      logical, dimension(:), allocatable :: mask1, mask2
      allocate(mask1(size(vec)))
      allocate(mask2(size(vec)))
      mask1 = .false.
      mask2 = .false.
      where(vec.eq.val1) mask1 = .true.
      where(vec.eq.val2) mask2 = .true.
      res = vec
      where(mask1) res = val2
      where(mask2) res = val1
      deallocate(mask1)
      deallocate(mask2)
    end function swap_i8

    function swap_r4(vec, val1, val2) result(res)
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec
      real(kind=r4), dimension(:), allocatable :: res
      real(kind=r4) :: val1, val2
      logical, dimension(:), allocatable :: mask1, mask2
      allocate(mask1(size(vec)))
      allocate(mask2(size(vec)))
      mask1 = .false.
      mask2 = .false.
      where(vec.eq.val1) mask1 = .true.
      where(vec.eq.val2) mask2 = .true.
      res = vec
      where(mask1) res = val2
      where(mask2) res = val1
      deallocate(mask1)
      deallocate(mask2)
    end function swap_r4

    function swap_r8(vec, val1, val2) result(res)
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec
      real(kind=r8), dimension(:), allocatable :: res
      real(kind=r8) :: val1, val2
      logical, dimension(:), allocatable :: mask1, mask2
      allocate(mask1(size(vec)))
      allocate(mask2(size(vec)))
      mask1 = .false.
      mask2 = .false.
      where(vec.eq.val1) mask1 = .true.
      where(vec.eq.val2) mask2 = .true.
      res = vec
      where(mask1) res = val2
      where(mask2) res = val1
      deallocate(mask1)
      deallocate(mask2)
    end function swap_r8

    function swap_r16(vec, val1, val2) result(res)
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec
      real(kind=r16), dimension(:), allocatable :: res
      real(kind=r16) :: val1, val2
      logical, dimension(:), allocatable :: mask1, mask2
      allocate(mask1(size(vec)))
      allocate(mask2(size(vec)))
      mask1 = .false.
      mask2 = .false.
      where(vec.eq.val1) mask1 = .true.
      where(vec.eq.val2) mask2 = .true.
      res = vec
      where(mask1) res = val2
      where(mask2) res = val1
      deallocate(mask1)
      deallocate(mask2)
    end function swap_r16

    function swap_c4(vec, val1, val2) result(res)
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r4), dimension(:), allocatable :: res
      complex(kind=r4) :: val1, val2
      logical, dimension(:), allocatable :: mask1, mask2
      allocate(mask1(size(vec)))
      allocate(mask2(size(vec)))
      mask1 = .false.
      mask2 = .false.
      where(vec.eq.val1) mask1 = .true.
      where(vec.eq.val2) mask2 = .true.
      res = vec
      where(mask1) res = val2
      where(mask2) res = val1
      deallocate(mask1)
      deallocate(mask2)
    end function swap_c4

    function swap_c8(vec, val1, val2) result(res)
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r8), dimension(:), allocatable :: res
      complex(kind=r8) :: val1, val2
      logical, dimension(:), allocatable :: mask1, mask2
      allocate(mask1(size(vec)))
      allocate(mask2(size(vec)))
      mask1 = .false.
      mask2 = .false.
      where(vec.eq.val1) mask1 = .true.
      where(vec.eq.val2) mask2 = .true.
      res = vec
      where(mask1) res = val2
      where(mask2) res = val1
      deallocate(mask1)
      deallocate(mask2)
    end function swap_c8

    function swap_c16(vec, val1, val2) result(res)
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec
      complex(kind=r16), dimension(:), allocatable :: res
      complex(kind=r16) :: val1, val2
      logical, dimension(:), allocatable :: mask1, mask2
      allocate(mask1(size(vec)))
      allocate(mask2(size(vec)))
      mask1 = .false.
      mask2 = .false.
      where(vec.eq.val1) mask1 = .true.
      where(vec.eq.val2) mask2 = .true.
      res = vec
      where(mask1) res = val2
      where(mask2) res = val1
      deallocate(mask1)
      deallocate(mask2)
    end function swap_c16

    function swap_str(vec, val1, val2) result(res)
      character(:), dimension(:), allocatable, intent(in) :: vec
      character(:), dimension(:), allocatable :: res
      character(*) :: val1, val2
      logical, dimension(:), allocatable :: mask1, mask2
      allocate(mask1(size(vec)))
      allocate(mask2(size(vec)))
      mask1 = .false.
      mask2 = .false.
      where(vec.eq.val1) mask1 = .true.
      where(vec.eq.val2) mask2 = .true.
      res = vec
      where(mask1) res = val2
      where(mask2) res = val1
      deallocate(mask1)
      deallocate(mask2)
    end function swap_str

end module vecfun

