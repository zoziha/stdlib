program example_exists

    use stdlib_io_path, only: exists
    implicit none
    integer :: fh

    open (newunit=fh, file='example_exists.txt')
    print *, exists('example_exists.txt')   ! .true.
    close (fh, status="delete")

    print *, exists('example_exists.txt')   ! .false.
    print *, exists('./doc/specs/')         ! Check if directory exists

end program example_exists
