module test_path

    use stdlib_io_path, only: exists
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_path

contains

    !> Collect all exported unit tests
    subroutine collect_path(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("path_exists", test_path_exists) &
                    ]

    end subroutine collect_path

    subroutine test_path_exists(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer :: fh

        open (newunit=fh, file='test_path_exists.txt')
        call check(error, exists('test_path_exists.txt'))
        close (fh, status="delete")

        call check(error,.not. exists('test_path_exists.txt.not'))

    end subroutine test_path_exists

end module test_path

program tester

    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_path, only: collect_path
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("path", collect_path) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program tester
