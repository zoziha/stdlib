
submodule (stdlib_io) stdlib_io_disp

    use, intrinsic :: iso_fortran_env, only: output_unit
    use stdlib_string_type, only: char, len
    use stdlib_strings, only: to_string
    implicit none

    type(string_type) :: coloum(5)

contains


    module subroutine disp_0_rsp(x, header, unit, brief, format, width, sep)
    
        real(sp), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        
        
        type(string_type), allocatable :: x_str
        
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                allocate(x_str)
                x_str = string_type(to_string(x, format_))
                write(unit_, "(A)") format_output_string([x_str], width_, brief_, sep_, len(x_str))
            
        
        
    end subroutine disp_0_rsp
    module subroutine disp_0_rdp(x, header, unit, brief, format, width, sep)
    
        real(dp), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        
        
        type(string_type), allocatable :: x_str
        
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                allocate(x_str)
                x_str = string_type(to_string(x, format_))
                write(unit_, "(A)") format_output_string([x_str], width_, brief_, sep_, len(x_str))
            
        
        
    end subroutine disp_0_rdp
    module subroutine disp_0_rqp(x, header, unit, brief, format, width, sep)
    
        real(qp), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        
        
        type(string_type), allocatable :: x_str
        
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                allocate(x_str)
                x_str = string_type(to_string(x, format_))
                write(unit_, "(A)") format_output_string([x_str], width_, brief_, sep_, len(x_str))
            
        
        
    end subroutine disp_0_rqp
    module subroutine disp_0_csp(x, header, unit, brief, format, width, sep)
    
        complex(sp), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        
        
        type(string_type), allocatable :: x_str
        
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                allocate(x_str)
                x_str = string_type(to_string(x, format_))
                write(unit_, "(A)") format_output_string([x_str], width_, brief_, sep_, len(x_str))
            
        
        
    end subroutine disp_0_csp
    module subroutine disp_0_cdp(x, header, unit, brief, format, width, sep)
    
        complex(dp), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        
        
        type(string_type), allocatable :: x_str
        
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                allocate(x_str)
                x_str = string_type(to_string(x, format_))
                write(unit_, "(A)") format_output_string([x_str], width_, brief_, sep_, len(x_str))
            
        
        
    end subroutine disp_0_cdp
    module subroutine disp_0_cqp(x, header, unit, brief, format, width, sep)
    
        complex(qp), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        
        
        type(string_type), allocatable :: x_str
        
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                allocate(x_str)
                x_str = string_type(to_string(x, format_))
                write(unit_, "(A)") format_output_string([x_str], width_, brief_, sep_, len(x_str))
            
        
        
    end subroutine disp_0_cqp
    module subroutine disp_0_iint8(x, header, unit, brief, format, width, sep)
    
        integer(int8), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        
        
        type(string_type), allocatable :: x_str
        
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                allocate(x_str)
                x_str = string_type(to_string(x, format_))
                write(unit_, "(A)") format_output_string([x_str], width_, brief_, sep_, len(x_str))
            
        
        
    end subroutine disp_0_iint8
    module subroutine disp_0_iint16(x, header, unit, brief, format, width, sep)
    
        integer(int16), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        
        
        type(string_type), allocatable :: x_str
        
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                allocate(x_str)
                x_str = string_type(to_string(x, format_))
                write(unit_, "(A)") format_output_string([x_str], width_, brief_, sep_, len(x_str))
            
        
        
    end subroutine disp_0_iint16
    module subroutine disp_0_iint32(x, header, unit, brief, format, width, sep)
    
        integer(int32), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        
        
        type(string_type), allocatable :: x_str
        
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                allocate(x_str)
                x_str = string_type(to_string(x, format_))
                write(unit_, "(A)") format_output_string([x_str], width_, brief_, sep_, len(x_str))
            
        
        
    end subroutine disp_0_iint32
    module subroutine disp_0_iint64(x, header, unit, brief, format, width, sep)
    
        integer(int64), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        
        
        type(string_type), allocatable :: x_str
        
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                allocate(x_str)
                x_str = string_type(to_string(x, format_))
                write(unit_, "(A)") format_output_string([x_str], width_, brief_, sep_, len(x_str))
            
        
        
    end subroutine disp_0_iint64
    module subroutine disp_0_llk(x, header, unit, brief, format, width, sep)
    
        logical(lk), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        
        
        type(string_type), allocatable :: x_str
        
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                allocate(x_str)
                x_str = string_type(to_string(x, format_))
                write(unit_, "(A)") format_output_string([x_str], width_, brief_, sep_, len(x_str))
            
        
        
    end subroutine disp_0_llk
    module subroutine disp_0_lc_bool(x, header, unit, brief, format, width, sep)
    
        logical(c_bool), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        
        
        type(string_type), allocatable :: x_str
        
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                allocate(x_str)
                x_str = string_type(to_string(x, format_))
                write(unit_, "(A)") format_output_string([x_str], width_, brief_, sep_, len(x_str))
            
        
        
    end subroutine disp_0_lc_bool
    module subroutine disp_0_tstring_type(x, header, unit, brief, format, width, sep)
    
        type(string_type), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        
        
        
        
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                write(unit_, "(A)") format_output_string([x], width_, brief_, sep_, len(x))
            
        
        
    end subroutine disp_0_tstring_type
    module subroutine disp_1_rsp(x, header, unit, brief, format, width, sep)
    
        real(sp), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        
        type(string_type), allocatable :: x_str(:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                array_info = array_info_maker(size(x, 1))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
            
                allocate(x_str(size(x, 1)))
                do i = 1, size(x, 1)
                    x_str(i) = string_type(to_string(x(i), format_))
                end do
                write(unit_, "(*(A))") format_output_string(x_str, width_, brief_, sep_, maxval(len(x_str)))
            
        
        
    end subroutine disp_1_rsp
    module subroutine disp_1_rdp(x, header, unit, brief, format, width, sep)
    
        real(dp), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        
        type(string_type), allocatable :: x_str(:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                array_info = array_info_maker(size(x, 1))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
            
                allocate(x_str(size(x, 1)))
                do i = 1, size(x, 1)
                    x_str(i) = string_type(to_string(x(i), format_))
                end do
                write(unit_, "(*(A))") format_output_string(x_str, width_, brief_, sep_, maxval(len(x_str)))
            
        
        
    end subroutine disp_1_rdp
    module subroutine disp_1_rqp(x, header, unit, brief, format, width, sep)
    
        real(qp), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        
        type(string_type), allocatable :: x_str(:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                array_info = array_info_maker(size(x, 1))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
            
                allocate(x_str(size(x, 1)))
                do i = 1, size(x, 1)
                    x_str(i) = string_type(to_string(x(i), format_))
                end do
                write(unit_, "(*(A))") format_output_string(x_str, width_, brief_, sep_, maxval(len(x_str)))
            
        
        
    end subroutine disp_1_rqp
    module subroutine disp_1_csp(x, header, unit, brief, format, width, sep)
    
        complex(sp), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        
        type(string_type), allocatable :: x_str(:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                array_info = array_info_maker(size(x, 1))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
            
                allocate(x_str(size(x, 1)))
                do i = 1, size(x, 1)
                    x_str(i) = string_type(to_string(x(i), format_))
                end do
                write(unit_, "(*(A))") format_output_string(x_str, width_, brief_, sep_, maxval(len(x_str)))
            
        
        
    end subroutine disp_1_csp
    module subroutine disp_1_cdp(x, header, unit, brief, format, width, sep)
    
        complex(dp), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        
        type(string_type), allocatable :: x_str(:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                array_info = array_info_maker(size(x, 1))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
            
                allocate(x_str(size(x, 1)))
                do i = 1, size(x, 1)
                    x_str(i) = string_type(to_string(x(i), format_))
                end do
                write(unit_, "(*(A))") format_output_string(x_str, width_, brief_, sep_, maxval(len(x_str)))
            
        
        
    end subroutine disp_1_cdp
    module subroutine disp_1_cqp(x, header, unit, brief, format, width, sep)
    
        complex(qp), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        
        type(string_type), allocatable :: x_str(:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                array_info = array_info_maker(size(x, 1))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
            
                allocate(x_str(size(x, 1)))
                do i = 1, size(x, 1)
                    x_str(i) = string_type(to_string(x(i), format_))
                end do
                write(unit_, "(*(A))") format_output_string(x_str, width_, brief_, sep_, maxval(len(x_str)))
            
        
        
    end subroutine disp_1_cqp
    module subroutine disp_1_iint8(x, header, unit, brief, format, width, sep)
    
        integer(int8), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        
        type(string_type), allocatable :: x_str(:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                array_info = array_info_maker(size(x, 1))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
            
                allocate(x_str(size(x, 1)))
                do i = 1, size(x, 1)
                    x_str(i) = string_type(to_string(x(i), format_))
                end do
                write(unit_, "(*(A))") format_output_string(x_str, width_, brief_, sep_, maxval(len(x_str)))
            
        
        
    end subroutine disp_1_iint8
    module subroutine disp_1_iint16(x, header, unit, brief, format, width, sep)
    
        integer(int16), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        
        type(string_type), allocatable :: x_str(:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                array_info = array_info_maker(size(x, 1))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
            
                allocate(x_str(size(x, 1)))
                do i = 1, size(x, 1)
                    x_str(i) = string_type(to_string(x(i), format_))
                end do
                write(unit_, "(*(A))") format_output_string(x_str, width_, brief_, sep_, maxval(len(x_str)))
            
        
        
    end subroutine disp_1_iint16
    module subroutine disp_1_iint32(x, header, unit, brief, format, width, sep)
    
        integer(int32), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        
        type(string_type), allocatable :: x_str(:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                array_info = array_info_maker(size(x, 1))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
            
                allocate(x_str(size(x, 1)))
                do i = 1, size(x, 1)
                    x_str(i) = string_type(to_string(x(i), format_))
                end do
                write(unit_, "(*(A))") format_output_string(x_str, width_, brief_, sep_, maxval(len(x_str)))
            
        
        
    end subroutine disp_1_iint32
    module subroutine disp_1_iint64(x, header, unit, brief, format, width, sep)
    
        integer(int64), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        
        type(string_type), allocatable :: x_str(:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                array_info = array_info_maker(size(x, 1))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
            
                allocate(x_str(size(x, 1)))
                do i = 1, size(x, 1)
                    x_str(i) = string_type(to_string(x(i), format_))
                end do
                write(unit_, "(*(A))") format_output_string(x_str, width_, brief_, sep_, maxval(len(x_str)))
            
        
        
    end subroutine disp_1_iint64
    module subroutine disp_1_llk(x, header, unit, brief, format, width, sep)
    
        logical(lk), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        
        type(string_type), allocatable :: x_str(:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                array_info = array_info_maker(size(x, 1))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
            
                allocate(x_str(size(x, 1)))
                do i = 1, size(x, 1)
                    x_str(i) = string_type(to_string(x(i), format_))
                end do
                write(unit_, "(*(A))") format_output_string(x_str, width_, brief_, sep_, maxval(len(x_str)))
            
        
        
    end subroutine disp_1_llk
    module subroutine disp_1_lc_bool(x, header, unit, brief, format, width, sep)
    
        logical(c_bool), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        
        type(string_type), allocatable :: x_str(:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                array_info = array_info_maker(size(x, 1))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
            
                allocate(x_str(size(x, 1)))
                do i = 1, size(x, 1)
                    x_str(i) = string_type(to_string(x(i), format_))
                end do
                write(unit_, "(*(A))") format_output_string(x_str, width_, brief_, sep_, maxval(len(x_str)))
            
        
        
    end subroutine disp_1_lc_bool
    module subroutine disp_1_tstring_type(x, header, unit, brief, format, width, sep)
    
        type(string_type), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        
        
        
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
                array_info = array_info_maker(size(x, 1))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
                
                write(unit_, "(*(A))") format_output_string(x, width_, brief_, sep_, maxval(len(x)))
                
        
        
    end subroutine disp_1_tstring_type
    module subroutine disp_2_rsp(x, header, unit, brief, format, width, sep)
    
        real(sp), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_, max_elem_len
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        integer :: j
        type(string_type), allocatable :: x_str(:,:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
            
                array_info = array_info_maker(size(x, 1), size(x, 2))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
                
                allocate(x_str(size(x, 1), size(x, 2)))
                do i = 1, size(x, 1)
                    do j = 1, size(x, 2)
                        x_str(i, j) = string_type(to_string(x(i, j), format_))
                    end do
                end do
                
                max_elem_len = maxval(len(x_str))
                
                if (brief_.and.size(x, 1)>5) then
                    do i = 1, 3
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                    
                    write(unit_, "(*(A))") format_output_string(coloum(1:merge(size(x, 2), 5, size(x, 2)<=5)), &
                        width_, brief_, sep_, max_elem_len)
                    write(unit_, "(*(A))") format_output_string(x_str(size(x, 1), :), width_, brief_, sep_, max_elem_len)
                
                else
                    do i = 1, size(x, 1)
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                end if
                
        
        
    end subroutine disp_2_rsp
    module subroutine disp_2_rdp(x, header, unit, brief, format, width, sep)
    
        real(dp), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_, max_elem_len
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        integer :: j
        type(string_type), allocatable :: x_str(:,:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
            
                array_info = array_info_maker(size(x, 1), size(x, 2))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
                
                allocate(x_str(size(x, 1), size(x, 2)))
                do i = 1, size(x, 1)
                    do j = 1, size(x, 2)
                        x_str(i, j) = string_type(to_string(x(i, j), format_))
                    end do
                end do
                
                max_elem_len = maxval(len(x_str))
                
                if (brief_.and.size(x, 1)>5) then
                    do i = 1, 3
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                    
                    write(unit_, "(*(A))") format_output_string(coloum(1:merge(size(x, 2), 5, size(x, 2)<=5)), &
                        width_, brief_, sep_, max_elem_len)
                    write(unit_, "(*(A))") format_output_string(x_str(size(x, 1), :), width_, brief_, sep_, max_elem_len)
                
                else
                    do i = 1, size(x, 1)
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                end if
                
        
        
    end subroutine disp_2_rdp
    module subroutine disp_2_rqp(x, header, unit, brief, format, width, sep)
    
        real(qp), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_, max_elem_len
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        integer :: j
        type(string_type), allocatable :: x_str(:,:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
            
                array_info = array_info_maker(size(x, 1), size(x, 2))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
                
                allocate(x_str(size(x, 1), size(x, 2)))
                do i = 1, size(x, 1)
                    do j = 1, size(x, 2)
                        x_str(i, j) = string_type(to_string(x(i, j), format_))
                    end do
                end do
                
                max_elem_len = maxval(len(x_str))
                
                if (brief_.and.size(x, 1)>5) then
                    do i = 1, 3
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                    
                    write(unit_, "(*(A))") format_output_string(coloum(1:merge(size(x, 2), 5, size(x, 2)<=5)), &
                        width_, brief_, sep_, max_elem_len)
                    write(unit_, "(*(A))") format_output_string(x_str(size(x, 1), :), width_, brief_, sep_, max_elem_len)
                
                else
                    do i = 1, size(x, 1)
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                end if
                
        
        
    end subroutine disp_2_rqp
    module subroutine disp_2_csp(x, header, unit, brief, format, width, sep)
    
        complex(sp), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_, max_elem_len
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        integer :: j
        type(string_type), allocatable :: x_str(:,:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
            
                array_info = array_info_maker(size(x, 1), size(x, 2))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
                
                allocate(x_str(size(x, 1), size(x, 2)))
                do i = 1, size(x, 1)
                    do j = 1, size(x, 2)
                        x_str(i, j) = string_type(to_string(x(i, j), format_))
                    end do
                end do
                
                max_elem_len = maxval(len(x_str))
                
                if (brief_.and.size(x, 1)>5) then
                    do i = 1, 3
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                    
                    write(unit_, "(*(A))") format_output_string(coloum(1:merge(size(x, 2), 5, size(x, 2)<=5)), &
                        width_, brief_, sep_, max_elem_len)
                    write(unit_, "(*(A))") format_output_string(x_str(size(x, 1), :), width_, brief_, sep_, max_elem_len)
                
                else
                    do i = 1, size(x, 1)
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                end if
                
        
        
    end subroutine disp_2_csp
    module subroutine disp_2_cdp(x, header, unit, brief, format, width, sep)
    
        complex(dp), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_, max_elem_len
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        integer :: j
        type(string_type), allocatable :: x_str(:,:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
            
                array_info = array_info_maker(size(x, 1), size(x, 2))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
                
                allocate(x_str(size(x, 1), size(x, 2)))
                do i = 1, size(x, 1)
                    do j = 1, size(x, 2)
                        x_str(i, j) = string_type(to_string(x(i, j), format_))
                    end do
                end do
                
                max_elem_len = maxval(len(x_str))
                
                if (brief_.and.size(x, 1)>5) then
                    do i = 1, 3
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                    
                    write(unit_, "(*(A))") format_output_string(coloum(1:merge(size(x, 2), 5, size(x, 2)<=5)), &
                        width_, brief_, sep_, max_elem_len)
                    write(unit_, "(*(A))") format_output_string(x_str(size(x, 1), :), width_, brief_, sep_, max_elem_len)
                
                else
                    do i = 1, size(x, 1)
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                end if
                
        
        
    end subroutine disp_2_cdp
    module subroutine disp_2_cqp(x, header, unit, brief, format, width, sep)
    
        complex(qp), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_, max_elem_len
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        integer :: j
        type(string_type), allocatable :: x_str(:,:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
            
                array_info = array_info_maker(size(x, 1), size(x, 2))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
                
                allocate(x_str(size(x, 1), size(x, 2)))
                do i = 1, size(x, 1)
                    do j = 1, size(x, 2)
                        x_str(i, j) = string_type(to_string(x(i, j), format_))
                    end do
                end do
                
                max_elem_len = maxval(len(x_str))
                
                if (brief_.and.size(x, 1)>5) then
                    do i = 1, 3
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                    
                    write(unit_, "(*(A))") format_output_string(coloum(1:merge(size(x, 2), 5, size(x, 2)<=5)), &
                        width_, brief_, sep_, max_elem_len)
                    write(unit_, "(*(A))") format_output_string(x_str(size(x, 1), :), width_, brief_, sep_, max_elem_len)
                
                else
                    do i = 1, size(x, 1)
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                end if
                
        
        
    end subroutine disp_2_cqp
    module subroutine disp_2_iint8(x, header, unit, brief, format, width, sep)
    
        integer(int8), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_, max_elem_len
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        integer :: j
        type(string_type), allocatable :: x_str(:,:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
            
                array_info = array_info_maker(size(x, 1), size(x, 2))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
                
                allocate(x_str(size(x, 1), size(x, 2)))
                do i = 1, size(x, 1)
                    do j = 1, size(x, 2)
                        x_str(i, j) = string_type(to_string(x(i, j), format_))
                    end do
                end do
                
                max_elem_len = maxval(len(x_str))
                
                if (brief_.and.size(x, 1)>5) then
                    do i = 1, 3
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                    
                    write(unit_, "(*(A))") format_output_string(coloum(1:merge(size(x, 2), 5, size(x, 2)<=5)), &
                        width_, brief_, sep_, max_elem_len)
                    write(unit_, "(*(A))") format_output_string(x_str(size(x, 1), :), width_, brief_, sep_, max_elem_len)
                
                else
                    do i = 1, size(x, 1)
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                end if
                
        
        
    end subroutine disp_2_iint8
    module subroutine disp_2_iint16(x, header, unit, brief, format, width, sep)
    
        integer(int16), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_, max_elem_len
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        integer :: j
        type(string_type), allocatable :: x_str(:,:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
            
                array_info = array_info_maker(size(x, 1), size(x, 2))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
                
                allocate(x_str(size(x, 1), size(x, 2)))
                do i = 1, size(x, 1)
                    do j = 1, size(x, 2)
                        x_str(i, j) = string_type(to_string(x(i, j), format_))
                    end do
                end do
                
                max_elem_len = maxval(len(x_str))
                
                if (brief_.and.size(x, 1)>5) then
                    do i = 1, 3
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                    
                    write(unit_, "(*(A))") format_output_string(coloum(1:merge(size(x, 2), 5, size(x, 2)<=5)), &
                        width_, brief_, sep_, max_elem_len)
                    write(unit_, "(*(A))") format_output_string(x_str(size(x, 1), :), width_, brief_, sep_, max_elem_len)
                
                else
                    do i = 1, size(x, 1)
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                end if
                
        
        
    end subroutine disp_2_iint16
    module subroutine disp_2_iint32(x, header, unit, brief, format, width, sep)
    
        integer(int32), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_, max_elem_len
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        integer :: j
        type(string_type), allocatable :: x_str(:,:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
            
                array_info = array_info_maker(size(x, 1), size(x, 2))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
                
                allocate(x_str(size(x, 1), size(x, 2)))
                do i = 1, size(x, 1)
                    do j = 1, size(x, 2)
                        x_str(i, j) = string_type(to_string(x(i, j), format_))
                    end do
                end do
                
                max_elem_len = maxval(len(x_str))
                
                if (brief_.and.size(x, 1)>5) then
                    do i = 1, 3
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                    
                    write(unit_, "(*(A))") format_output_string(coloum(1:merge(size(x, 2), 5, size(x, 2)<=5)), &
                        width_, brief_, sep_, max_elem_len)
                    write(unit_, "(*(A))") format_output_string(x_str(size(x, 1), :), width_, brief_, sep_, max_elem_len)
                
                else
                    do i = 1, size(x, 1)
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                end if
                
        
        
    end subroutine disp_2_iint32
    module subroutine disp_2_iint64(x, header, unit, brief, format, width, sep)
    
        integer(int64), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_, max_elem_len
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        integer :: j
        type(string_type), allocatable :: x_str(:,:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
            
                array_info = array_info_maker(size(x, 1), size(x, 2))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
                
                allocate(x_str(size(x, 1), size(x, 2)))
                do i = 1, size(x, 1)
                    do j = 1, size(x, 2)
                        x_str(i, j) = string_type(to_string(x(i, j), format_))
                    end do
                end do
                
                max_elem_len = maxval(len(x_str))
                
                if (brief_.and.size(x, 1)>5) then
                    do i = 1, 3
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                    
                    write(unit_, "(*(A))") format_output_string(coloum(1:merge(size(x, 2), 5, size(x, 2)<=5)), &
                        width_, brief_, sep_, max_elem_len)
                    write(unit_, "(*(A))") format_output_string(x_str(size(x, 1), :), width_, brief_, sep_, max_elem_len)
                
                else
                    do i = 1, size(x, 1)
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                end if
                
        
        
    end subroutine disp_2_iint64
    module subroutine disp_2_llk(x, header, unit, brief, format, width, sep)
    
        logical(lk), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_, max_elem_len
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        integer :: j
        type(string_type), allocatable :: x_str(:,:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
            
                array_info = array_info_maker(size(x, 1), size(x, 2))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
                
                allocate(x_str(size(x, 1), size(x, 2)))
                do i = 1, size(x, 1)
                    do j = 1, size(x, 2)
                        x_str(i, j) = string_type(to_string(x(i, j), format_))
                    end do
                end do
                
                max_elem_len = maxval(len(x_str))
                
                if (brief_.and.size(x, 1)>5) then
                    do i = 1, 3
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                    
                    write(unit_, "(*(A))") format_output_string(coloum(1:merge(size(x, 2), 5, size(x, 2)<=5)), &
                        width_, brief_, sep_, max_elem_len)
                    write(unit_, "(*(A))") format_output_string(x_str(size(x, 1), :), width_, brief_, sep_, max_elem_len)
                
                else
                    do i = 1, size(x, 1)
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                end if
                
        
        
    end subroutine disp_2_llk
    module subroutine disp_2_lc_bool(x, header, unit, brief, format, width, sep)
    
        logical(c_bool), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_, max_elem_len
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        integer :: j
        type(string_type), allocatable :: x_str(:,:)
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
            
                array_info = array_info_maker(size(x, 1), size(x, 2))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
                
                allocate(x_str(size(x, 1), size(x, 2)))
                do i = 1, size(x, 1)
                    do j = 1, size(x, 2)
                        x_str(i, j) = string_type(to_string(x(i, j), format_))
                    end do
                end do
                
                max_elem_len = maxval(len(x_str))
                
                if (brief_.and.size(x, 1)>5) then
                    do i = 1, 3
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                    
                    write(unit_, "(*(A))") format_output_string(coloum(1:merge(size(x, 2), 5, size(x, 2)<=5)), &
                        width_, brief_, sep_, max_elem_len)
                    write(unit_, "(*(A))") format_output_string(x_str(size(x, 1), :), width_, brief_, sep_, max_elem_len)
                
                else
                    do i = 1, size(x, 1)
                        write(unit_, "(*(A))") format_output_string(x_str(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                end if
                
        
        
    end subroutine disp_2_lc_bool
    module subroutine disp_2_tstring_type(x, header, unit, brief, format, width, sep)
    
        type(string_type), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_, max_elem_len
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i
        
        
        type(string_type) :: array_info
        
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        
        sep_    = optval(sep, "  ")
        coloum  = string_type(":")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        
            
                array_info = array_info_maker(size(x, 1), size(x, 2))
                write(unit_, "(A)") format_output_string([array_info], width_, brief_, "", len(array_info))
                
                max_elem_len = maxval(len(x))
                
                if (brief_.and.size(x, 1)>5) then
                    do i = 1, 3
                        write(unit_, "(*(A))") format_output_string(x(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                    
                    write(unit_, "(*(A))") format_output_string(coloum(1:merge(size(x, 2), 5, size(x, 2)<=5)), &
                        width_, brief_, sep_, max_elem_len)
                    write(unit_, "(*(A))") format_output_string(x(size(x, 1), :), width_, brief_, sep_, max_elem_len)
                    
                else
                    do i = 1, size(x, 1)
                        write(unit_, "(*(A))") format_output_string(x(i, :), width_, brief_, sep_, max_elem_len)
                    end do
                end if
                
        
        
    end subroutine disp_2_tstring_type
    
    !> Format output string
    pure function format_output_string(x, width, brief, sep, max_elem_len) result(str)
    
        type(string_type), intent(in) :: x(:)
        integer, intent(in) :: width
        logical, intent(in) :: brief
        character(len=*), intent(in) :: sep
        
        integer, intent(in) :: max_elem_len
        
        character(merge((max(max_elem_len, 2)+len(sep))*min(size(x, 1), 5), width, brief)+2), allocatable :: str(:)
        
        character(:), allocatable :: buffer
        
        character(max(max_elem_len, 2)+len(sep)) :: elem_buffer
        integer :: elem_len, num1, num2, i, j
        
        if (brief) then
        
            allocate(str(1))
            buffer = ""
            
            if (size(x, 1) <= 5) then
            
                do i = 1, size(x, 1)
                    elem_buffer = char(x(i))//sep
                    buffer = buffer//elem_buffer
                end do
            
            else
                
                do i = 1, 3
                    elem_buffer = char(x(i))//sep
                    buffer = buffer//elem_buffer
                end do
                
                elem_buffer = ".."//sep
                buffer = buffer//elem_buffer
                
                elem_buffer = char(x(size(x, 1)))//sep
                buffer = buffer//elem_buffer
                
            end if
            
            str(1) = buffer
        
        else
        
            elem_len = len(elem_buffer)
            
            num1 = merge(width/elem_len, 1, elem_len <= width)
            num2 = size(x, 1)/num1
            
            if (num2 > 1 .or. size(x, 1) > 1) then
                allocate(str(merge(num2, num2 + 1, mod(size(x, 1), num1)==0)))
                
                do i = 1, size(str) - 1
                    
                    buffer = ""
                    do j = 1, num1
                        elem_buffer = char(x((i-1)*num1+j))//sep
                        buffer = buffer//elem_buffer
                    end do
                    
                    if ( len(x((i-1)*num1+j-1)) > width-len(sep)-1 ) then
                        buffer(width-len(sep)-1:) = "**"//repeat(" ", len(sep))
                    end if
                    
                    str(i) = buffer
                    
                    str(i)(width+1:) = "&"//new_line("")
                    
                end do
                
                buffer = ""
                do j = 1, merge(num1, mod(size(x, 1), num1), mod(size(x, 1), num1)==0)
                    elem_buffer = char(x((i-1)*num1+j))//sep
                    buffer = buffer//elem_buffer
                end do
                
                if ( len(x((i-1)*num1+j-1)) > width-len(sep)-1 ) then
                    buffer(width-len(sep)-1:) = "**"//repeat(" ", len(sep))
                end if
                
                str(i) = buffer
            
            else
            
                allocate(str(1))
                buffer = ""
                do j = 1, size(x, 1)
                    elem_buffer = char(x(j))//sep
                    buffer = buffer//elem_buffer
                end do
                
                if ( len(x(j-1)) > width-len(sep)-1 ) then
                    buffer(width-len(sep)-1:) = "**"//repeat(" ", len(sep))
                end if
                
                str(1) = buffer
                
            end if
            
        end if
        
    end function format_output_string
    
    !> Print array information
    pure type(string_type) function array_info_maker(m, n) result(info)
        integer, intent(in) :: m
        integer, intent(in), optional :: n
        
        if (present(n)) then
            info = string_type('[matrix size: ' // to_string(m) // '×' // to_string(n) // ']')
        else
            info = string_type('[vector size: ' // to_string(m) // ']')
        end if
        
    end function array_info_maker
    
    !> Display `character(*)` value.
    module subroutine disp_char(x, header, unit, brief, format, width, sep)
    
        character(*), intent(in), optional :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: x_, sep_
        
        !> State default values
        x_      = optval(x, "")
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        width_  = optval(width, 80)
        width_  = merge(width_, 80, width_ > 80)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, "(A)") format_output_string([string_type(header)], width_, brief_, "", len(header))
        end if
        
        coloum(1) = string_type(x_)
        write(unit_, "(A)") format_output_string(coloum(1:1), width_, brief_, sep_, len(coloum(1)))
        
    end subroutine disp_char

end submodule stdlib_io_disp