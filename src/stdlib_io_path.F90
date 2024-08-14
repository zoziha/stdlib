! SPDX-Identifier: MIT

!> This module implements some useful functions on pathnames.
!> To read or write files see [open()](../page/specs/stdlib_io.html#open),
!> and for accessing the filesystem see the [os](../page/specs/stdlib_io.html) module.
!> ([Specification](../page/specs/stdlib_io.html))
module stdlib_io_path

    implicit none
    private

    public :: exists

contains

    !> Version: experimental
    !>
    !> Check if a file or directory exists
    !> ([Specification](../page/specs/stdlib_io.html#exists))
    logical function exists(path)
        character(len=*), intent(in) :: path

        inquire (file=path, exist=exists)
#ifdef __INTEL_COMPILER
        if (.not. exists) inquire (directory=file, exist=exists)
#endif

    end function exists

end module stdlib_io_path
