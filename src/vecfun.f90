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

  interface push
    !! adds val pushed to the end of the input vector
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
    !! adds val pushed to idx of the input vector
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
    !! adds val pushed to the end of the input vector
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
    !! delete the last element in the input vector or if idx is provided
    !! then delete the element at index idx
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

  contains

    subroutine push_i1(vec, val)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i1), intent(in) :: val
      vec = [vec, val]
    end subroutine push_i1

    subroutine push_i2(vec, val)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i2), intent(in) :: val
      vec = [vec, val]
    end subroutine push_i2

    subroutine push_i4(vec, val)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i4), intent(in) :: val
      vec = [vec, val]
    end subroutine push_i4

    subroutine push_i8(vec, val)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i8), intent(in) :: val
      vec = [vec, val]
    end subroutine push_i8

    subroutine push_r4(vec, val)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r4), intent(in) :: val
      vec = [vec, val]
    end subroutine push_r4

    subroutine push_r8(vec, val)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r8), intent(in) :: val
      vec = [vec, val]
    end subroutine push_r8

    subroutine push_r16(vec, val)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r16), intent(in) :: val
      vec = [vec, val]
    end subroutine push_r16

    subroutine push_c4(vec, val)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r4), intent(in) :: val
      vec = [vec, val]
    end subroutine push_c4

    subroutine push_c8(vec, val)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r8), intent(in) :: val
      vec = [vec, val]
    end subroutine push_c8

    subroutine push_c16(vec, val)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r16), intent(in) :: val
      vec = [vec, val]
    end subroutine push_c16

    subroutine push_str(vec, val)
      character(:), dimension(:), allocatable, intent(inout) :: vec
      character(*), intent(in) :: val
      vec = [vec, val]
    end subroutine push_str

    subroutine pushto_i1(vec, val, idx)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i1), intent(in) :: val
      integer, intent(in) :: idx
      vec = [vec(:idx-1), val, vec(idx:)]
    end subroutine pushto_i1

    subroutine pushto_i2(vec, val, idx)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i2), intent(in) :: val
      integer, intent(in) :: idx
      vec = [vec(:idx-1), val, vec(idx:)]
    end subroutine pushto_i2

    subroutine pushto_i4(vec, val, idx)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i4), intent(in) :: val
      integer, intent(in) :: idx
      vec = [vec(:idx-1), val, vec(idx:)]
    end subroutine pushto_i4

    subroutine pushto_i8(vec, val, idx)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i8), intent(in) :: val
      integer, intent(in) :: idx
      vec = [vec(:idx-1), val, vec(idx:)]
    end subroutine pushto_i8

    subroutine pushto_r4(vec, val, idx)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r4), intent(in) :: val
      integer, intent(in) :: idx
      vec = [vec(:idx-1), val, vec(idx:)]
    end subroutine pushto_r4

    subroutine pushto_r8(vec, val, idx)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r8), intent(in) :: val
      integer, intent(in) :: idx
      vec = [vec(:idx-1), val, vec(idx:)]
    end subroutine pushto_r8

    subroutine pushto_r16(vec, val, idx)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r16), intent(in) :: val
      integer, intent(in) :: idx
      vec = [vec(:idx-1), val, vec(idx:)]
    end subroutine pushto_r16

    subroutine pushto_c4(vec, val, idx)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r4), intent(in) :: val
      integer, intent(in) :: idx
      vec = [vec(:idx-1), val, vec(idx:)]
    end subroutine pushto_c4

    subroutine pushto_c8(vec, val, idx)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r8), intent(in) :: val
      integer, intent(in) :: idx
      vec = [vec(:idx-1), val, vec(idx:)]
    end subroutine pushto_c8

    subroutine pushto_c16(vec, val, idx)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r16), intent(in) :: val
      integer, intent(in) :: idx
      vec = [vec(:idx-1), val, vec(idx:)]
    end subroutine pushto_c16

    subroutine pushto_str(vec, val, idx)
      character(:), dimension(:), allocatable, intent(inout) :: vec
      character(*), intent(in) :: val
      integer, intent(in) :: idx
      vec = [vec(:idx-1), val, vec(idx:)]
    end subroutine pushto_str

    subroutine pushnew_i1(vec, val)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i1), intent(in) :: val
      if (.not.any(vec.eq.val)) vec = [vec, val]
    end subroutine pushnew_i1

    subroutine pushnew_i2(vec, val)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i2), intent(in) :: val
      if (.not.any(vec.eq.val)) vec = [vec, val]
    end subroutine pushnew_i2

    subroutine pushnew_i4(vec, val)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i4), intent(in) :: val
      if (.not.any(vec.eq.val)) vec = [vec, val]
    end subroutine pushnew_i4

    subroutine pushnew_i8(vec, val)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i8), intent(in) :: val
      if (.not.any(vec.eq.val)) vec = [vec, val]
    end subroutine pushnew_i8

    subroutine pushnew_r4(vec, val)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r4), intent(in) :: val
      if (.not.any(vec.eq.val)) vec = [vec, val]
    end subroutine pushnew_r4

    subroutine pushnew_r8(vec, val)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r8), intent(in) :: val
      if (.not.any(vec.eq.val)) vec = [vec, val]
    end subroutine pushnew_r8

    subroutine pushnew_r16(vec, val)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r16), intent(in) :: val
      if (.not.any(vec.eq.val)) vec = [vec, val]
    end subroutine pushnew_r16

    subroutine pushnew_c4(vec, val)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r4), intent(in) :: val
      if (.not.any(vec.eq.val)) vec = [vec, val]
    end subroutine pushnew_c4

    subroutine pushnew_c8(vec, val)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r8), intent(in) :: val
      if (.not.any(vec.eq.val)) vec = [vec, val]
    end subroutine pushnew_c8

    subroutine pushnew_c16(vec, val)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r16), intent(in) :: val
      if (.not.any(vec.eq.val)) vec = [vec, val]
    end subroutine pushnew_c16

    subroutine pushnew_str(vec, val)
      character(:), dimension(:), allocatable, intent(inout) :: vec
      character(*), intent(in) :: val
      if (.not.any(vec.eq.val)) vec = [vec, val]
    end subroutine pushnew_str

    subroutine pop_i1(vec, idx)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        vec = [vec(:tmp-1), vec(tmp+1:)]
      else
        vec = [vec(:tmp-1)]
      end if
    end subroutine pop_i1

    subroutine pop_i2(vec, idx)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        vec = [vec(:tmp-1), vec(tmp+1:)]
      else
        vec = [vec(:tmp-1)]
      end if
    end subroutine pop_i2

    subroutine pop_i4(vec, idx)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        vec = [vec(:tmp-1), vec(tmp+1:)]
      else
        vec = [vec(:tmp-1)]
      end if
    end subroutine pop_i4

    subroutine pop_i8(vec, idx)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        vec = [vec(:tmp-1), vec(tmp+1:)]
      else
        vec = [vec(:tmp-1)]
      end if
    end subroutine pop_i8

    subroutine pop_r4(vec, idx)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        vec = [vec(:tmp-1), vec(tmp+1:)]
      else
        vec = [vec(:tmp-1)]
      end if
    end subroutine pop_r4

    subroutine pop_r8(vec, idx)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        vec = [vec(:tmp-1), vec(tmp+1:)]
      else
        vec = [vec(:tmp-1)]
      end if
    end subroutine pop_r8

    subroutine pop_r16(vec, idx)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        vec = [vec(:tmp-1), vec(tmp+1:)]
      else
        vec = [vec(:tmp-1)]
      end if
    end subroutine pop_r16

    subroutine pop_c4(vec, idx)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        vec = [vec(:tmp-1), vec(tmp+1:)]
      else
        vec = [vec(:tmp-1)]
      end if
    end subroutine pop_c4

    subroutine pop_c8(vec, idx)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        vec = [vec(:tmp-1), vec(tmp+1:)]
      else
        vec = [vec(:tmp-1)]
      end if
    end subroutine pop_c8

    subroutine pop_c16(vec, idx)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        vec = [vec(:tmp-1), vec(tmp+1:)]
      else
        vec = [vec(:tmp-1)]
      end if
    end subroutine pop_c16

    subroutine pop_str(vec, idx)
      character(:), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in), optional :: idx
      integer :: tmp
      if (.not.present(idx)) then
        tmp = size(vec)
      else
        tmp = idx
      end if
      if (tmp.le.size(vec)) then
        vec = [vec(:tmp-1), vec(tmp+1:)]
      else
        vec = [vec(:tmp-1)]
      end if
    end subroutine pop_str

    subroutine popval_i1(vec, val)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i1), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end subroutine popval_i1

    subroutine popval_i2(vec, val)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i2), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end subroutine popval_i2

    subroutine popval_i4(vec, val)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i4), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end subroutine popval_i4

    subroutine popval_i8(vec, val)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i8), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end subroutine popval_i8

    subroutine popval_r4(vec, val)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r4), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val, 1)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end subroutine popval_r4

    subroutine popval_r8(vec, val)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r8), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end subroutine popval_r8

    subroutine popval_r16(vec, val)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r16), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end subroutine popval_r16

    subroutine popval_c4(vec, val)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r4), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end subroutine popval_c4

    subroutine popval_c8(vec, val)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r8), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end subroutine popval_c8

    subroutine popval_c16(vec, val)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r16), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end subroutine popval_c16

    subroutine popval_str(vec, val)
      character(:), dimension(:), allocatable, intent(inout) :: vec
      character(*), intent(in) :: val
      integer, dimension(1) :: tmp
      integer :: idx
      tmp = findloc(vec, val)
      idx = tmp(1)
      vec = [vec(:idx-1), vec(idx:)]
    end subroutine popval_str

    subroutine popall_i1(vec, val)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i1), intent(in) :: val
      do while (any(vec.eq.val))
        call popval(vec, val)
      end do
    end subroutine popall_i1

    subroutine popall_i2(vec, val)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i2), intent(in) :: val
      do while (any(vec.eq.val))
        call popval(vec, val)
      end do
    end subroutine popall_i2

    subroutine popall_i4(vec, val)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i4), intent(in) :: val
      do while (any(vec.eq.val))
         call popval(vec, val)
      end do
    end subroutine popall_i4

    subroutine popall_i8(vec, val)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i8), intent(in) :: val
      do while (any(vec.eq.val))
        call popval(vec, val)
      end do
    end subroutine popall_i8

    subroutine popall_r4(vec, val)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r4), intent(in) :: val
      do while (any(vec.eq.val))
        call popval(vec, val)
      end do
    end subroutine popall_r4

    subroutine popall_r8(vec, val)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r8), intent(in) :: val
      do while (any(vec.eq.val))
        call popval(vec, val)
      end do
    end subroutine popall_r8

    subroutine popall_r16(vec, val)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r16), intent(in) :: val
      do while (any(vec.eq.val))
        call popval(vec, val)
      end do
    end subroutine popall_r16

    subroutine popall_c4(vec, val)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r4), intent(in) :: val
      do while (any(vec.eq.val))
        call popval(vec, val)
      end do
    end subroutine popall_c4

    subroutine popall_c8(vec, val)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r8), intent(in) :: val
      do while (any(vec.eq.val))
        call popval(vec, val)
      end do
    end subroutine popall_c8

    subroutine popall_c16(vec, val)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r16), intent(in) :: val
      do while (any(vec.eq.val))
        call popval(vec, val)
      end do
    end subroutine popall_c16

    subroutine popall_str(vec, val)
      character(:), dimension(:), allocatable, intent(inout) :: vec
      character(*), intent(in) :: val
      do while (any(vec.eq.val))
        call popval(vec, val)
      end do
    end subroutine popall_str

    subroutine concat_i1(vec1, vec2)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec1
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec2
      vec1 = [vec1, vec2]
    end subroutine concat_i1

    subroutine concat_i2(vec1, vec2)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec1
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec2
      vec1 = [vec1, vec2]
    end subroutine concat_i2

    subroutine concat_i4(vec1, vec2)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec1
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec2
      vec1 = [vec1, vec2]
    end subroutine concat_i4

    subroutine concat_i8(vec1, vec2)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec1
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec2
      vec1 = [vec1, vec2]
    end subroutine concat_i8

    subroutine concat_r4(vec1, vec2)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec1
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec2
      vec1 = [vec1, vec2]
    end subroutine concat_r4

    subroutine concat_r8(vec1, vec2)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec1
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec2
      vec1 = [vec1, vec2]
    end subroutine concat_r8

    subroutine concat_r16(vec1, vec2)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec1
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec2
      vec1 = [vec1, vec2]
    end subroutine concat_r16

    subroutine concat_c4(vec1, vec2)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec1
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec2
      vec1 = [vec1, vec2]
    end subroutine concat_c4

    subroutine concat_c8(vec1, vec2)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec1
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec2
      vec1 = [vec1, vec2]
    end subroutine concat_c8

    subroutine concat_c16(vec1, vec2)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec1
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec2
      vec1 = [vec1, vec2]
    end subroutine concat_c16

    subroutine concat_str(vec1, vec2)
      character(:), dimension(:), allocatable, intent(inout) :: vec1
      character(:), dimension(:), allocatable, intent(in) :: vec2
      vec1 = [vec1, vec2]
    end subroutine concat_str

    subroutine echo_i1(vec, val)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i1), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      do i = 1, val
        vec = [vec, tmp]
      end do
    end subroutine echo_i1

    subroutine echo_i2(vec, val)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i2), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      do i = 1, val
        vec = [vec, tmp]
      end do
    end subroutine echo_i2

    subroutine echo_i4(vec, val)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i4), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      do i = 1, val
        vec = [vec, tmp]
      end do
    end subroutine echo_i4

    subroutine echo_i8(vec, val)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i8), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      do i = 1, val
        vec = [vec, tmp]
      end do
    end subroutine echo_i8

    subroutine echo_r4(vec, val)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r4), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      do i = 1, val
        vec = [vec, tmp]
      end do
    end subroutine echo_r4

    subroutine echo_r8(vec, val)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r8), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      do i = 1, val
        vec = [vec, tmp]
      end do
    end subroutine echo_r8

    subroutine echo_r16(vec, val)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r16), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      do i = 1, val
        vec = [vec, tmp]
      end do
    end subroutine echo_r16

    subroutine echo_c4(vec, val)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r4), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      do i = 1, val
        vec = [vec, tmp]
      end do
    end subroutine echo_c4

    subroutine echo_c8(vec, val)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r8), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      do i = 1, val
        vec = [vec, tmp]
      end do
    end subroutine echo_c8

    subroutine echo_c16(vec, val)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r16), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      do i = 1, val
        vec = [vec, tmp]
      end do
    end subroutine echo_c16

    subroutine echo_str(vec, val)
      character(:), dimension(:), allocatable, intent(inout) :: vec
      character(:), dimension(:), allocatable :: tmp
      integer, intent(in) :: val
      integer :: i
      tmp = vec
      do i = 1, val
        vec = [vec, tmp]
      end do
    end subroutine echo_str

    subroutine unique_i1(vec, sorted)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i1), dimension(:), allocatable :: tmp
      logical, intent(in) :: sorted
      integer(kind=i1) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        tmp = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            call push(tmp, prev)
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
          if (.not. any(tmp.eq.prev)) call push(tmp, prev)
        end do
      end if
      vec = tmp
    end subroutine unique_i1

    subroutine unique_i2(vec, sorted)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i2), dimension(:), allocatable :: tmp
      logical, intent(in) :: sorted
      integer(kind=i2) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        tmp = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            call push(tmp, prev)
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
          if (.not. any(tmp.eq.prev)) call push(tmp, prev)
        end do
      end if
      vec = tmp
    end subroutine unique_i2

    subroutine unique_i4(vec, sorted)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i4), dimension(:), allocatable :: tmp
      logical, intent(in) :: sorted
      integer(kind=i4) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        tmp = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            call push(tmp, prev)
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
          if (.not. any(tmp.eq.prev)) call push(tmp, prev)
        end do
      end if
      vec = tmp
    end subroutine unique_i4

    subroutine unique_i8(vec, sorted)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i8), dimension(:), allocatable :: tmp
      logical, intent(in) :: sorted
      integer(kind=i8) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        tmp = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            call push(tmp, prev)
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
          if (.not. any(tmp.eq.prev)) call push(tmp, prev)
        end do
      end if
      vec = tmp
    end subroutine unique_i8

    subroutine unique_r4(vec, sorted)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r4), dimension(:), allocatable :: tmp
      logical, intent(in) :: sorted
      real(kind=r4) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        tmp = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            call push(tmp, prev)
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
          if (.not. any(tmp.eq.prev)) call push(tmp, prev)
        end do
      end if
      vec = tmp
    end subroutine unique_r4

    subroutine unique_r8(vec, sorted)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r8), dimension(:), allocatable :: tmp
      logical, intent(in) :: sorted
      real(kind=r8) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        tmp = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            call push(tmp, prev)
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
          if (.not. any(tmp.eq.prev)) call push(tmp, prev)
        end do
      end if
      vec = tmp
    end subroutine unique_r8

    subroutine unique_r16(vec, sorted)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r16), dimension(:), allocatable :: tmp
      logical, intent(in) :: sorted
      real(kind=r16) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        tmp = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            call push(tmp, prev)
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
          if (.not. any(tmp.eq.prev)) call push(tmp, prev)
        end do
      end if
      vec = tmp
    end subroutine unique_r16

    subroutine unique_c4(vec, sorted)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r4), dimension(:), allocatable :: tmp
      logical, intent(in) :: sorted
      complex(kind=r4) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        tmp = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            call push(tmp, prev)
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
          if (.not. any(tmp.eq.prev)) call push(tmp, prev)
        end do
      end if
      vec = tmp
    end subroutine unique_c4

    subroutine unique_c8(vec, sorted)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r8), dimension(:), allocatable :: tmp
      logical, intent(in) :: sorted
      complex(kind=r8) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        tmp = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            call push(tmp, prev)
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
          if (.not. any(tmp.eq.prev)) call push(tmp, prev)
        end do
      end if
      vec = tmp
    end subroutine unique_c8

    subroutine unique_c16(vec, sorted)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r16), dimension(:), allocatable :: tmp
      logical, intent(in) :: sorted
      complex(kind=r16) :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        tmp = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            call push(tmp, prev)
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
          if (.not. any(tmp.eq.prev)) call push(tmp, prev)
        end do
      end if
      vec = tmp
    end subroutine unique_c16

    subroutine unique_str(vec, sorted)
      character(:), dimension(:), allocatable, intent(inout) :: vec
      character(:), dimension(:), allocatable :: tmp
      logical, intent(in) :: sorted
      character(:), allocatable :: prev
      integer :: i, j, k
      if (sorted) then
        j = lbound(vec, 1)
        prev = vec(j)
        tmp = [prev]
        j = lbound(vec, 1) + 1
        k = ubound(vec, 1)
        do i = j, k
          if (vec(i).ne.prev) then
            prev = vec(i)
            call push(tmp, prev)
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
          if (.not. any(tmp.eq.prev)) call push(tmp, prev)
        end do
      end if
      vec = tmp
    end subroutine unique_str

    subroutine reverse_i1(vec)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i1) :: tmp
      integer :: j, k
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      do while (j.le.k)
        tmp = vec(j)
        vec(j) = vec(k)
        vec(k) = tmp
        j = j + 1
        k = k - 1
      end do
    end subroutine reverse_i1

    subroutine reverse_i2(vec)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i2) :: tmp
      integer :: j, k
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      do while (j.le.k)
        tmp = vec(j)
        vec(j) = vec(k)
        vec(k) = tmp
        j = j + 1
        k = k - 1
      end do
    end subroutine reverse_i2

    subroutine reverse_i4(vec)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i4) :: tmp
      integer :: j, k
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      do while (j.le.k)
        tmp = vec(j)
        vec(j) = vec(k)
        vec(k) = tmp
        j = j + 1
        k = k - 1
      end do
    end subroutine reverse_i4

    subroutine reverse_i8(vec)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer(kind=i8) :: tmp
      integer :: j, k
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      do while (j.le.k)
        tmp = vec(j)
        vec(j) = vec(k)
        vec(k) = tmp
        j = j + 1
        k = k - 1
      end do
    end subroutine reverse_i8

    subroutine reverse_r4(vec)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r4) :: tmp
      integer :: j, k
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      do while (j.le.k)
        tmp = vec(j)
        vec(j) = vec(k)
        vec(k) = tmp
        j = j + 1
        k = k - 1
      end do
    end subroutine reverse_r4

    subroutine reverse_r8(vec)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r8) :: tmp
      integer :: j, k
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      do while (j.le.k)
        tmp = vec(j)
        vec(j) = vec(k)
        vec(k) = tmp
        j = j + 1
        k = k - 1
      end do
    end subroutine reverse_r8

    subroutine reverse_r16(vec)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      real(kind=r16) :: tmp
      integer :: j, k
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      do while (j.le.k)
        tmp = vec(j)
        vec(j) = vec(k)
        vec(k) = tmp
        j = j + 1
        k = k - 1
      end do
    end subroutine reverse_r16

    subroutine reverse_c4(vec)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r4) :: tmp
      integer :: j, k
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      do while (j.le.k)
        tmp = vec(j)
        vec(j) = vec(k)
        vec(k) = tmp
        j = j + 1
        k = k - 1
      end do
    end subroutine reverse_c4

    subroutine reverse_c8(vec)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r8) :: tmp
      integer :: j, k
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      do while (j.le.k)
        tmp = vec(j)
        vec(j) = vec(k)
        vec(k) = tmp
        j = j + 1
        k = k - 1
      end do
    end subroutine reverse_c8

    subroutine reverse_c16(vec)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      complex(kind=r16) :: tmp
      integer :: j, k
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      do while (j.le.k)
        tmp = vec(j)
        vec(j) = vec(k)
        vec(k) = tmp
        j = j + 1
        k = k - 1
      end do
    end subroutine reverse_c16

    subroutine reverse_str(vec)
      character(:), dimension(:), allocatable, intent(inout) :: vec
      character(:), allocatable :: tmp
      integer :: j, k
      j = lbound(vec, 1)
      k = ubound(vec, 1)
      do while (j.le.k)
        tmp = vec(j)
        vec(j) = vec(k)
        vec(k) = tmp
        j = j + 1
        k = k - 1
      end do
    end subroutine reverse_str

    subroutine every_i1(vec, val)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in) :: val
      integer(kind=i1), allocatable :: tmp(:)
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      tmp = [vec(j)]
      do i = j+val, k, val
        call push(tmp, vec(i))
      end do
      vec = tmp
     end subroutine every_i1

    subroutine every_i2(vec, val)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in) :: val
      integer(kind=i2), allocatable :: tmp(:)
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      tmp = [vec(j)]
      do i = j+val, k, val
        call push(tmp, vec(i))
      end do
      vec = tmp
     end subroutine every_i2

    subroutine every_i4(vec, val)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in) :: val
      integer(kind=i4), allocatable :: tmp(:)
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      tmp = [vec(j)]
      do i = j+val, k, val
        call push(tmp, vec(i))
      end do
      vec = tmp
     end subroutine every_i4

    subroutine every_i8(vec, val)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in) :: val
      integer(kind=i8), allocatable :: tmp(:)
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      tmp = [vec(j)]
      do i = j+val, k, val
        call push(tmp, vec(i))
      end do
      vec = tmp
     end subroutine every_i8

    subroutine every_r4(vec, val)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in) :: val
      real(kind=r4), allocatable :: tmp(:)
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      tmp = [vec(j)]
      do i = j+val, k, val
        call push(tmp, vec(i))
      end do
      vec = tmp
     end subroutine every_r4

    subroutine every_r8(vec, val)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in) :: val
      real(kind=r8), allocatable :: tmp(:)
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      tmp = [vec(j)]
      do i = j+val, k, val
        call push(tmp, vec(i))
      end do
      vec = tmp
     end subroutine every_r8

    subroutine every_r16(vec, val)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in) :: val
      real(kind=r16), allocatable :: tmp(:)
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      tmp = [vec(j)]
      do i = j+val, k, val
        call push(tmp, vec(i))
      end do
      vec = tmp
     end subroutine every_r16

    subroutine every_c4(vec, val)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in) :: val
      complex(kind=r4), allocatable :: tmp(:)
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      tmp = [vec(j)]
      do i = j+val, k, val
        call push(tmp, vec(i))
      end do
      vec = tmp
     end subroutine every_c4

    subroutine every_c8(vec, val)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in) :: val
      complex(kind=r8), allocatable :: tmp(:)
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      tmp = [vec(j)]
      do i = j+val, k, val
        call push(tmp, vec(i))
      end do
      vec = tmp
     end subroutine every_c8

    subroutine every_c16(vec, val)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in) :: val
      complex(kind=r16), allocatable :: tmp(:)
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      tmp = [vec(j)]
      do i = j+val, k, val
        call push(tmp, vec(i))
      end do
      vec = tmp
    end subroutine every_c16

    subroutine every_str(vec, val)
      character(:), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in) :: val
      character(:), allocatable :: tmp(:)
      integer :: i, j, k
      j = val
      k = ubound(vec, 1)
      tmp = [vec(j)]
      do i = j+val, k, val
        call push(tmp, vec(i))
      end do
      vec = tmp
    end subroutine every_str

    subroutine zip_i1(vec1, vec2)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec1
      integer(kind=i1), dimension(:), allocatable, intent(in) :: vec2
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      do i = j, k
        call pushto(vec1, vec2(i), 2*i)
      end do
    end subroutine zip_i1

    subroutine zip_i2(vec1, vec2)
      integer(kind=i2), dimension(:), allocatable, intent(inout) :: vec1
      integer(kind=i2), dimension(:), allocatable, intent(in) :: vec2
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      do i = j, k
        call pushto(vec1, vec2(i), 2*i)
      end do
    end subroutine zip_i2

    subroutine zip_i4(vec1, vec2)
      integer(kind=i4), dimension(:), allocatable, intent(inout) :: vec1
      integer(kind=i4), dimension(:), allocatable, intent(in) :: vec2
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      do i = j, k
        call pushto(vec1, vec2(i), 2*i)
      end do
    end subroutine zip_i4

    subroutine zip_i8(vec1, vec2)
      integer(kind=i8), dimension(:), allocatable, intent(inout) :: vec1
      integer(kind=i8), dimension(:), allocatable, intent(in) :: vec2
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      do i = j, k
        call pushto(vec1, vec2(i), 2*i)
      end do
    end subroutine zip_i8

    subroutine zip_r4(vec1, vec2)
      real(kind=r4), dimension(:), allocatable, intent(inout) :: vec1
      real(kind=r4), dimension(:), allocatable, intent(in) :: vec2
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      do i = j, k
        call pushto(vec1, vec2(i), 2*i)
      end do
    end subroutine zip_r4

    subroutine zip_r8(vec1, vec2)
      real(kind=r8), dimension(:), allocatable, intent(inout) :: vec1
      real(kind=r8), dimension(:), allocatable, intent(in) :: vec2
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      do i = j, k
        call pushto(vec1, vec2(i), 2*i)
      end do
    end subroutine zip_r8

    subroutine zip_r16(vec1, vec2)
      real(kind=r16), dimension(:), allocatable, intent(inout) :: vec1
      real(kind=r16), dimension(:), allocatable, intent(in) :: vec2
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      do i = j, k
        call pushto(vec1, vec2(i), 2*i)
      end do
    end subroutine zip_r16

    subroutine zip_c4(vec1, vec2)
      complex(kind=r4), dimension(:), allocatable, intent(inout) :: vec1
      complex(kind=r4), dimension(:), allocatable, intent(in) :: vec2
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      do i = j, k
        call pushto(vec1, vec2(i), 2*i)
      end do
    end subroutine zip_c4

    subroutine zip_c8(vec1, vec2)
      complex(kind=r8), dimension(:), allocatable, intent(inout) :: vec1
      complex(kind=r8), dimension(:), allocatable, intent(in) :: vec2
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      do i = j, k
        call pushto(vec1, vec2(i), 2*i)
      end do
    end subroutine zip_c8

    subroutine zip_c16(vec1, vec2)
      complex(kind=r16), dimension(:), allocatable, intent(inout) :: vec1
      complex(kind=r16), dimension(:), allocatable, intent(in) :: vec2
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      do i = j, k
        call pushto(vec1, vec2(i), 2*i)
      end do
    end subroutine zip_c16

    subroutine zip_str(vec1, vec2)
      character(:), dimension(:), allocatable, intent(inout) :: vec1
      character(:), dimension(:), allocatable, intent(in) :: vec2
      integer :: i, j, k
      j = lbound(vec2, 1)
      k = ubound(vec2, 1)
      do i = j, k
        call pushto(vec1, vec2(i), 2*i)
      end do
    end subroutine zip_str

    subroutine popevery_i1(vec, val)
      integer(kind=i1), dimension(:), allocatable, intent(inout) :: vec
      integer, intent(in) :: val
      integer :: i, j, k
      j = lbound(vec, 1)
      k = (ubound(vec, 1) - j)/val
      do i = j, k, val
        call pop(vec, i-j+val)
      end do
    end subroutine popevery_i1

end module vecfun

