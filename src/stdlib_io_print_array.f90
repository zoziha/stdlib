submodule(stdlib_io) stdlib_io_print_array

    use, intrinsic :: iso_fortran_env, only: output_unit
    implicit none

contains

    module subroutine print_array_rsp(array, unit, fmt, delimiter, brief)
        real(sp), intent(in) :: array(:, :)
        integer, intent(in), optional :: unit
        character(len=*), intent(in), optional :: fmt
        character(len=1), intent(in), optional :: delimiter
        logical, intent(in), optional :: brief

        integer :: i, j, unit_, shape_(2)
        character(len=:), allocatable :: fmt_
        character(len=1) :: delimiter_
        character(len=3) :: delim_str
        logical :: brief_

        shape_ = shape(array)
        if (any(shape_ == 0)) return
        unit_ = optval(unit, output_unit)
        delimiter_ = optval(delimiter, delimiter_default)
        delim_str = "'"//delimiter_//"'"
        brief_ = optval(brief, .true.)
        if (present(fmt)) then
            fmt_ = "(*"//fmt(1:len(fmt) - 1)//",:,"//delim_str//"))"
        else
                fmt_ = "(*"//FMT_REAL_sp (1:len(FMT_REAL_sp) - 1)//",:,"//delim_str//"))"
        end if

        if (brief_) then

            if (shape_(1) > 5) then
                if (shape_(2) > 5) then
                    do i = 1, 3
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_, advance='no') array(shape_(1), :3)
                    write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                    write (unit_, fmt=fmt_) array(shape_(1), shape_(2))
                else
                    do i = 1, 3
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_) array(shape_(1), :)

                end if
            else
                if (shape_(2) > 5) then
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                else
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                end if
            end if

        else

            do i = 1, shape_(1)
                write (unit_, fmt=fmt_) array(i, :)
            end do

        end if

    end subroutine print_array_rsp
    module subroutine print_array_rdp(array, unit, fmt, delimiter, brief)
        real(dp), intent(in) :: array(:, :)
        integer, intent(in), optional :: unit
        character(len=*), intent(in), optional :: fmt
        character(len=1), intent(in), optional :: delimiter
        logical, intent(in), optional :: brief

        integer :: i, j, unit_, shape_(2)
        character(len=:), allocatable :: fmt_
        character(len=1) :: delimiter_
        character(len=3) :: delim_str
        logical :: brief_

        shape_ = shape(array)
        if (any(shape_ == 0)) return
        unit_ = optval(unit, output_unit)
        delimiter_ = optval(delimiter, delimiter_default)
        delim_str = "'"//delimiter_//"'"
        brief_ = optval(brief, .true.)
        if (present(fmt)) then
            fmt_ = "(*"//fmt(1:len(fmt) - 1)//",:,"//delim_str//"))"
        else
                fmt_ = "(*"//FMT_REAL_dp (1:len(FMT_REAL_dp) - 1)//",:,"//delim_str//"))"
        end if

        if (brief_) then

            if (shape_(1) > 5) then
                if (shape_(2) > 5) then
                    do i = 1, 3
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_, advance='no') array(shape_(1), :3)
                    write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                    write (unit_, fmt=fmt_) array(shape_(1), shape_(2))
                else
                    do i = 1, 3
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_) array(shape_(1), :)

                end if
            else
                if (shape_(2) > 5) then
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                else
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                end if
            end if

        else

            do i = 1, shape_(1)
                write (unit_, fmt=fmt_) array(i, :)
            end do

        end if

    end subroutine print_array_rdp
    module subroutine print_array_csp(array, unit, fmt, delimiter, brief)
        complex(sp), intent(in) :: array(:, :)
        integer, intent(in), optional :: unit
        character(len=*), intent(in), optional :: fmt
        character(len=1), intent(in), optional :: delimiter
        logical, intent(in), optional :: brief

        integer :: i, j, unit_, shape_(2)
        character(len=:), allocatable :: fmt_
        character(len=1) :: delimiter_
        character(len=3) :: delim_str
        logical :: brief_

        shape_ = shape(array)
        if (any(shape_ == 0)) return
        unit_ = optval(unit, output_unit)
        delimiter_ = optval(delimiter, delimiter_default)
        delim_str = "'"//delimiter_//"'"
        brief_ = optval(brief, .true.)
        if (present(fmt)) then
            fmt_ = "(*"//fmt(1:len(fmt) - 1)//",:,"//delim_str//"))"
        else
                fmt_ = "(*"//FMT_COMPLEX_sp (1:11)//delim_str//FMT_COMPLEX_sp (14:23)//",:,"//delim_str//"))"
        end if

        if (brief_) then

            if (shape_(1) > 5) then
                if (shape_(2) > 5) then
                    do i = 1, 3
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_, advance='no') array(shape_(1), :3)
                    write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                    write (unit_, fmt=fmt_) array(shape_(1), shape_(2))
                else
                    do i = 1, 3
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_) array(shape_(1), :)

                end if
            else
                if (shape_(2) > 5) then
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                else
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                end if
            end if

        else

            do i = 1, shape_(1)
                write (unit_, fmt=fmt_) array(i, :)
            end do

        end if

    end subroutine print_array_csp
    module subroutine print_array_cdp(array, unit, fmt, delimiter, brief)
        complex(dp), intent(in) :: array(:, :)
        integer, intent(in), optional :: unit
        character(len=*), intent(in), optional :: fmt
        character(len=1), intent(in), optional :: delimiter
        logical, intent(in), optional :: brief

        integer :: i, j, unit_, shape_(2)
        character(len=:), allocatable :: fmt_
        character(len=1) :: delimiter_
        character(len=3) :: delim_str
        logical :: brief_

        shape_ = shape(array)
        if (any(shape_ == 0)) return
        unit_ = optval(unit, output_unit)
        delimiter_ = optval(delimiter, delimiter_default)
        delim_str = "'"//delimiter_//"'"
        brief_ = optval(brief, .true.)
        if (present(fmt)) then
            fmt_ = "(*"//fmt(1:len(fmt) - 1)//",:,"//delim_str//"))"
        else
                fmt_ = "(*"//FMT_COMPLEX_dp (1:11)//delim_str//FMT_COMPLEX_dp (14:23)//",:,"//delim_str//"))"
        end if

        if (brief_) then

            if (shape_(1) > 5) then
                if (shape_(2) > 5) then
                    do i = 1, 3
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_, advance='no') array(shape_(1), :3)
                    write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                    write (unit_, fmt=fmt_) array(shape_(1), shape_(2))
                else
                    do i = 1, 3
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_) array(shape_(1), :)

                end if
            else
                if (shape_(2) > 5) then
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                else
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                end if
            end if

        else

            do i = 1, shape_(1)
                write (unit_, fmt=fmt_) array(i, :)
            end do

        end if

    end subroutine print_array_cdp
    module subroutine print_array_iint8(array, unit, fmt, delimiter, brief)
        integer(int8), intent(in) :: array(:, :)
        integer, intent(in), optional :: unit
        character(len=*), intent(in), optional :: fmt
        character(len=1), intent(in), optional :: delimiter
        logical, intent(in), optional :: brief

        integer :: i, j, unit_, shape_(2)
        character(len=:), allocatable :: fmt_
        character(len=1) :: delimiter_
        character(len=3) :: delim_str
        logical :: brief_

        shape_ = shape(array)
        if (any(shape_ == 0)) return
        unit_ = optval(unit, output_unit)
        delimiter_ = optval(delimiter, delimiter_default)
        delim_str = "'"//delimiter_//"'"
        brief_ = optval(brief, .true.)
        if (present(fmt)) then
            fmt_ = "(*"//fmt(1:len(fmt) - 1)//",:,"//delim_str//"))"
        else
                fmt_ = "(*"//FMT_INT(1:len(FMT_INT) - 1)//",:,"//delim_str//"))"
        end if

        if (brief_) then

            if (shape_(1) > 5) then
                if (shape_(2) > 5) then
                    do i = 1, 3
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_, advance='no') array(shape_(1), :3)
                    write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                    write (unit_, fmt=fmt_) array(shape_(1), shape_(2))
                else
                    do i = 1, 3
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_) array(shape_(1), :)

                end if
            else
                if (shape_(2) > 5) then
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                else
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                end if
            end if

        else

            do i = 1, shape_(1)
                write (unit_, fmt=fmt_) array(i, :)
            end do

        end if

    end subroutine print_array_iint8
    module subroutine print_array_iint16(array, unit, fmt, delimiter, brief)
        integer(int16), intent(in) :: array(:, :)
        integer, intent(in), optional :: unit
        character(len=*), intent(in), optional :: fmt
        character(len=1), intent(in), optional :: delimiter
        logical, intent(in), optional :: brief

        integer :: i, j, unit_, shape_(2)
        character(len=:), allocatable :: fmt_
        character(len=1) :: delimiter_
        character(len=3) :: delim_str
        logical :: brief_

        shape_ = shape(array)
        if (any(shape_ == 0)) return
        unit_ = optval(unit, output_unit)
        delimiter_ = optval(delimiter, delimiter_default)
        delim_str = "'"//delimiter_//"'"
        brief_ = optval(brief, .true.)
        if (present(fmt)) then
            fmt_ = "(*"//fmt(1:len(fmt) - 1)//",:,"//delim_str//"))"
        else
                fmt_ = "(*"//FMT_INT(1:len(FMT_INT) - 1)//",:,"//delim_str//"))"
        end if

        if (brief_) then

            if (shape_(1) > 5) then
                if (shape_(2) > 5) then
                    do i = 1, 3
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_, advance='no') array(shape_(1), :3)
                    write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                    write (unit_, fmt=fmt_) array(shape_(1), shape_(2))
                else
                    do i = 1, 3
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_) array(shape_(1), :)

                end if
            else
                if (shape_(2) > 5) then
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                else
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                end if
            end if

        else

            do i = 1, shape_(1)
                write (unit_, fmt=fmt_) array(i, :)
            end do

        end if

    end subroutine print_array_iint16
    module subroutine print_array_iint32(array, unit, fmt, delimiter, brief)
        integer(int32), intent(in) :: array(:, :)
        integer, intent(in), optional :: unit
        character(len=*), intent(in), optional :: fmt
        character(len=1), intent(in), optional :: delimiter
        logical, intent(in), optional :: brief

        integer :: i, j, unit_, shape_(2)
        character(len=:), allocatable :: fmt_
        character(len=1) :: delimiter_
        character(len=3) :: delim_str
        logical :: brief_

        shape_ = shape(array)
        if (any(shape_ == 0)) return
        unit_ = optval(unit, output_unit)
        delimiter_ = optval(delimiter, delimiter_default)
        delim_str = "'"//delimiter_//"'"
        brief_ = optval(brief, .true.)
        if (present(fmt)) then
            fmt_ = "(*"//fmt(1:len(fmt) - 1)//",:,"//delim_str//"))"
        else
                fmt_ = "(*"//FMT_INT(1:len(FMT_INT) - 1)//",:,"//delim_str//"))"
        end if

        if (brief_) then

            if (shape_(1) > 5) then
                if (shape_(2) > 5) then
                    do i = 1, 3
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_, advance='no') array(shape_(1), :3)
                    write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                    write (unit_, fmt=fmt_) array(shape_(1), shape_(2))
                else
                    do i = 1, 3
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_) array(shape_(1), :)

                end if
            else
                if (shape_(2) > 5) then
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                else
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                end if
            end if

        else

            do i = 1, shape_(1)
                write (unit_, fmt=fmt_) array(i, :)
            end do

        end if

    end subroutine print_array_iint32
    module subroutine print_array_iint64(array, unit, fmt, delimiter, brief)
        integer(int64), intent(in) :: array(:, :)
        integer, intent(in), optional :: unit
        character(len=*), intent(in), optional :: fmt
        character(len=1), intent(in), optional :: delimiter
        logical, intent(in), optional :: brief

        integer :: i, j, unit_, shape_(2)
        character(len=:), allocatable :: fmt_
        character(len=1) :: delimiter_
        character(len=3) :: delim_str
        logical :: brief_

        shape_ = shape(array)
        if (any(shape_ == 0)) return
        unit_ = optval(unit, output_unit)
        delimiter_ = optval(delimiter, delimiter_default)
        delim_str = "'"//delimiter_//"'"
        brief_ = optval(brief, .true.)
        if (present(fmt)) then
            fmt_ = "(*"//fmt(1:len(fmt) - 1)//",:,"//delim_str//"))"
        else
                fmt_ = "(*"//FMT_INT(1:len(FMT_INT) - 1)//",:,"//delim_str//"))"
        end if

        if (brief_) then

            if (shape_(1) > 5) then
                if (shape_(2) > 5) then
                    do i = 1, 3
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_, advance='no') array(shape_(1), :3)
                    write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                    write (unit_, fmt=fmt_) array(shape_(1), shape_(2))
                else
                    do i = 1, 3
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                    write (unit_, fmt='(a)') ":"
                    write (unit_, fmt=fmt_) array(shape_(1), :)

                end if
            else
                if (shape_(2) > 5) then
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_, advance='no') array(i, :3)
                        write (unit_, fmt='(a)', advance='no') delimiter_//"..."//delimiter_
                        write (unit_, fmt=fmt_) array(i, shape_(2))
                    end do
                else
                    do i = 1, shape_(1)
                        write (unit_, fmt=fmt_) array(i, :)
                    end do
                end if
            end if

        else

            do i = 1, shape_(1)
                write (unit_, fmt=fmt_) array(i, :)
            end do

        end if

    end subroutine print_array_iint64

end submodule stdlib_io_print_array
