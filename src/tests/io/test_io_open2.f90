program test_io_open2

    use stdlib_io, only: open
    use stdlib_strings, only: to_string

    i = open("1.txt", "w")
    write(i, "(A30)") to_string((1.0,3.0))
    close(i)

    call system("cat 1.txt")

    open(2, file="2.txt", access="stream", form="formatted")
    write(2, "(A30)") to_string((1.0,3.0))
    close(2)

    call system("cat 2.txt")

    open(3, file="3.txt", access="sequential")
    write(3, "(A30)") to_string((1.0,3.0))
    close(3)

end program test_io_open2