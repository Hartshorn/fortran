program reference

! Fortran is always pass-by-reference
! use subroutines for side-effects
! use only pure functions
! see 'alter_them' below for impure

    use types

    implicit none

100 FORMAT(a,i3)


    type(value) :: x, y
    integer :: n, i
    integer, dimension(0:4) :: x_arr, y_arr
    
    write(*,*) 'AT THE TOP, TESTING'
    call x%print
    call y%print
    
    call x%init(10, .true.)
    call y%init(20, .true.)
    
    write(*,*) 'AFTER VALUES ARE SETUP'
    call x%print
    call y%print

    
    x_arr = (/ (i, i = 1,5) /)
    y_arr = (/ 1, 2, 3, 4, 5 /)

    write(*,*) 'Array Section:'

    do i = 0,4
        write(*,*) x_arr(i)
    end do


    n = add_them(x,y)

    write(*, 100) "The value of n is:", n

contains

pure function add_them(x, y) result(r)

    type(value), intent(in) :: x, y
    integer :: r

    if (x%has_value .and. y%has_value) then
        r = x%how_much + y%how_much
    else
        r = 0
    end if

end function add_them

end program reference
