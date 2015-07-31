program evolution

    use functions
    use types

    implicit none

    integer, parameter :: WIDTH = 100, HEIGHT = 30, PLT_NRG = 80, REPR_NRG = 100
    integer, parameter :: MAX_ARR_SIZE = WIDTH * HEIGHT

    integer :: i

    type(Animal), dimension(0:MAX_ARR_SIZE) :: Animals
    type(Plant), dimension(0:MAX_ARR_SIZE)  :: Plants

    type(Animal) :: a, b
    type(Plant)  :: p

    a%alive = .true.
    a%x     = 10
    a%y     = 10

    b = a%copy()

    b%x = 1
    b%y = 2

    call a%eat(PLT_NRG)

    p%alive = .true.
    p%x     = 12
    p%y     = 13

    Animals(0) = a
    Animals(1) = b
    Plants(0)  = p

    write(*,*) "ANIMALS:"
    do i = 0, MAX_ARR_SIZE
        if (Animals(i)%alive) then
            call Animals(i)%show
        end if
    end do

    write(*,*) "PLANTS:"
    do i = 0, MAX_ARR_SIZE
        if (Plants(i)%alive) then
            call Plants(i)%show
        end if
    end do



end program evolution
