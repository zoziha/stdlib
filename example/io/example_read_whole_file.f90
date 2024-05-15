program example_read_whole_file
    use stdlib_io, only: read_whole_file
    implicit none
    character(:), allocatable :: line
    character(*), parameter :: file = '../example/io/example_read_whole_file.f90'

    call read_whole_file(file, line)
    write (*, "(a)") line

end program example_read_whole_file
