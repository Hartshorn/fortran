program evolution

! Fortran is always pass-by-reference
! use subroutines for side-effects
! use only pure functions
! see 'alter_them' below for impure

    implicit none

100 FORMAT(a,i3)
200 FORMAT(a)

    type value
        logical :: has_value = .false.
        integer :: how_much = 0
    end type value

    type(value) :: x, y
    integer :: n, m, i
    integer, dimension(0:4) :: x_arr, y_arr

    x_arr = (/ (i, i = 1,5) /)
    y_arr = (/ 1, 2, 3, 4, 5 /)

    write(*,*) 'Array Section:'

    do i = 0,4
        write(*,*) x_arr(i)
    end do

    call setup_values(x, y, 10, 20)

    n = add_them(x,y)

    write(*, 100) "The value of n is:", n

    write(*,200) 'Alter them?'

    m = alter_them(x, y)

    write(*, 100) "The value of m is:", m

    n = add_them(x, y)

    write(*, 100) "The value of n is now:", n

    write(*, 200) 'Lets write some numbers'

    call n_in_a_row(n)

contains

subroutine setup_values(x, y, n, m)

    type(value) :: x, y
    integer :: n, m

    x%has_value = .true.
    y%has_value = .true.

    x%how_much = n
    y%how_much = m

end subroutine setup_values

integer function add_them(x, y)

    type(value) :: x, y

    if (x%has_value .and. y%has_value) then
        add_them = x%how_much + y%how_much
    else
        add_them = 0
    end if

end function add_them

subroutine n_in_a_row(n)

    integer :: i, n

    do i = 0, n
        write(*,*) i
    end do

end subroutine n_in_a_row

integer function alter_them(x, y)

    type(value) :: x, y

    x%how_much = 10
    y%how_much = 10

    alter_them = 12

end function alter_them

end program evolution
