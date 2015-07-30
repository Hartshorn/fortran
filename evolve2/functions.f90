module functions

    use types

    implicit none

contains

subroutine add_animal(a, x, y)

    type(organism) :: a(:,:)
    integer        :: x, y

    call generate_genes(a(x,y))
    a(x,y)%has_animal = .true.
    a(x,y)%dir = 1
    a(x,y)%energy = 1000

end subroutine add_animal

subroutine add_plant(a, x, y)

    type(organism) :: a(:,:)
    integer        :: x, y

    a(x,y)%has_plant = .true.

end subroutine add_plant

subroutine generate_genes(a)

    real :: n
    type(organism) :: a

    do i = 1, size(a%genes)
        call random_number(n)
        a%genes(i) = int(n * 10) + 1
    end do

end subroutine generate_genes

subroutine move_animal(ar, a, w, h)

    type(organism) :: ar(:,:), a

!   What are the x and y here used for?  Do they need to be here?

    integer :: x = 0, y = 0, w, h, new_x, new_y, hold_x, hold_y

    if (a%dir >= 2 .and. a%dir < 5) then
        new_x = 1
    else if (a%dir == 1 .or. a%dir == 5) then
        new_x = 0
    else
        new_x = -1
    end if

    if (a%dir >= 0 .and. a%dir < 3) then
        new_y = -1
    else if (a%dir >= 4 .or. a%dir < 7) then
        new_y = 1
    else
        new_y = 0
    end if

    hold_x = (x + new_x + w)
    hold_y = (y + new_y + h)

    new_x = mod((x + new_x + w), w) + 1
    new_y = mod((y + new_y + h), h) + 1

    a%has_animal = .false.

!   dealing with just an organism makes this not work
!   how to move it?

    ar(new_x, new_y)%has_animal = .true.
    ar(new_x, new_y)%genes = a%genes
    ar(new_x, new_y)%dir = a%dir
    ar(new_x, new_y)%energy = a%energy - 1

end subroutine move_animal

subroutine turn_animal(a)

    type(organism) :: a
    real    :: n
    integer :: val

    call random_number(n)
    val = int(n * sum(a%genes))

    print *, 'VAL:', val

    if (maxval(a%genes) > val) then
        a%dir = abs(mod(a%dir + 1, 8))
    else
        a%dir = abs(mod(a%dir - 1, 8))
    end if

end subroutine turn_animal

subroutine eat_animal(a)

    type(organism) :: a
    integer :: pe = 80

    if (a%has_animal .and. a%has_plant) then
        a%energy = a%energy + pe
        a%has_plant = .false.
    end if

end subroutine eat_animal

logical function reproduce_animal(a)

    type(organism) :: a
    integer        :: re = 200

    if (a%energy >= re) then
        a%energy = a%energy / 2
        reproduce_animal = .true.
    else
        reproduce_animal = .false.
    end if

end function reproduce_animal

subroutine make_another(a)

    type(organism) :: a

    a%has_animal = .true.

!   This wont work with the new organism only version.

    !a(x + 1, y + 1)%energy = a(x,y)%energy
    !a(x + 1, y + 1)%genes = mutate_genes(a(x,y)%genes)
    !a(x + 1, y + 1)%dir = mod(a(x,y)%dir + 1, 8)

end subroutine make_another

function mutate_genes(g)

    integer :: g(8), mutate_genes(8)
    real :: n, o

    call random_number(n)
    call random_number(o)

    g(int(n * 10)) = g(int(n * 10)) + mod(int(o * 10), 10)

    mutate_genes = g

end function mutate_genes

subroutine simulate_day(a, w, h)

    type(organism) :: a(:,:)
    integer :: i, j, w, h

    !call add_random_plant(in jungle)
    !call add_random_plant(out of jungle)

    do i = 1, w
        do j = 1, h
            if (a(i,j)%has_animal .and. (a(i,j)%energy >= 1)) then
                call turn_animal(a(i,j))
                call move_animal(a, a(i,j), w, h)
                call eat_animal(a(i, j))
                if (reproduce_animal(a(i, j))) then
                    !call make_another(a(i, j))
                end if
            end if
            if (a(i,j)%energy < 1) then
                a(i,j)%has_animal = .false.
            end if
        end do
    end do
end subroutine simulate_day

subroutine draw_world(a, w, h)

    type(organism) :: a(:,:)
    integer :: i, j, w, h

    do j = 1, h
        write(*, '(a)', advance="no") '|'
        do i = 1, w
            if (a(i,j)%has_animal) then
                write(*, '(a)', advance="no") 'M'
            else if (a(i,j)%has_plant) then
                write(*, '(a)', advance="no") '*'
            else
                write(*, '(a)', advance="no") ' '
            end if
        end do
        write(*, '(a)') '|'
    end do

end subroutine draw_world

subroutine init_random_seed()

    integer :: t, sz, clock
    integer, dimension(:), allocatable :: seed

    call random_seed(size = sz)
    allocate(seed(sz))
    call system_clock(count = clock)
    seed = clock + 37 * (/(t - 1, t = 1, sz)/)
    call random_seed(put = seed)
    deallocate(seed)

end subroutine init_random_seed

integer function get_days()

    write(*, '(a)', advance="no") 'Days: '
    read(*, *) get_days

end function get_days

subroutine show_all(a, w, h)

    type(organism) :: a(:,:)
    integer :: w, h, i, j

    do i = 1, w
        do j = 1, h
            if (a(i,j)%has_animal) then
                call show_organism(a(i,j))
            end if
        end do
    end do

end subroutine show_all

end module functions
