module functions

use types

implicit none

contains

subroutine init_animal(a)
    
    type(animal), intent(inout) :: a
    integer :: i
    
    a%dead = .false.
    a%energy = 1000
    a%x = 1
    a%y = 1
    a%dir = 7
    call generate_genes(a)
!    a%genes = [(i, i=1,8)]
    
end subroutine init_animal

subroutine show_animal(a, i)

100     FORMAT(a, i2, a, i2, a2)
200     FORMAT(a, 8i4)

    type(animal), intent(in) :: a
    integer :: i, n
    
    write(*, '(a, i3)') 'Animal: ', i
    write(*, '(a, L)') 'Dead: ', a%dead
    write(*, 200) 'Energy: ', a%energy
    write(*, 100) 'Location: (', a%x, ',', a%y, ')'
    write(*, 200) 'Direction: ', a%dir
    write(*, 200, advance="no") 'Genes: ', (a%genes(n), n=1,size(a%genes))
    write(*,*)
    write(*,*)

end subroutine show_animal

subroutine show_plant(p, i)

100     FORMAT(a, i2, a, i2, a2)

    type(plant) :: p
    integer :: i
    
    write(*, '(a, i3)') 'Plant: ', i
    write(*, '(a, L)') 'Dead: ', p%dead
    write(*, 100) 'Location: (', p%x, ',', p%y, ')'
    write(*, *)

end subroutine show_plant

subroutine init_plant_array(p)

    type(plant) :: p(:)
    integer :: i
    
    do i = 1, size(p)
        p(i)%dead = .true.
    end do

end subroutine init_plant_array

subroutine init_animal_array(a)

    type(animal) :: a(:)
    integer :: i
    
    do i = 1, size(a)
        a(i)%dead = .true.
    end do

end subroutine init_animal_array
    

subroutine move_animal(a, w, h)

    type(animal) :: a
    integer :: w, h, new_x, new_y
    
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
    
    a%x = mod(a%x + new_x + w, w) + 1
    a%y = mod(a%y + new_y + h, h) + 1
    a%energy = a%energy - 1
    
end subroutine move_animal


subroutine generate_genes(a)

    real                 :: hold(1:8)
    integer              :: vals(1:8), k, i
    integer, allocatable :: seed(:)
    type(animal) :: a
    
    call date_and_time(values = vals)

    call random_seed(size = k)
    allocate(seed(1:k))
    seed(:) = vals(8)
    
    call random_seed(put = seed)
    call random_number(hold)
    
    
    do i = 1, size(a%genes)
        a%genes(i) = mod(int(hold(i) * 10), 100)
    end do

end subroutine generate_genes

subroutine turn_animal(a)
    
    type(animal) :: a
    
    if (a%genes(mod(a%x + a%y, 8)) > 5) then
        a%dir = a%dir + 1
    else
        a%dir = a%dir - 1
    end if

end subroutine turn_animal

subroutine eat_animal(a, p, pe)

    integer :: i, pe
    type(animal) :: a
    type(plant)  :: p(:)
    
    do i = 1, size(p)
        if (a%x == p(i)%x .and. a%y == p(i)%y) then
            a%energy = a%energy + pe
            p(i)%dead = .true.
        end if
    end do

end subroutine eat_animal

logical function reproduce_animal(a, re)

    type(animal) :: a
    integer :: re
    
    if (a%energy >= re) then
        a%energy = a%energy / 2
        reproduce_animal = .true.
    else
        reproduce_animal = .false.
    end if
        
end function reproduce_animal

type(animal) function copy_animal(a)
    
    type(animal) :: a, b
    
    b = a
    b%x = b%x + 1
    b%y = b%y + 1
    b%dir = mod(a%dir + 1, 8)
    b%energy = 1000
    call mutate_genes(b)
    
    copy_animal = b

end function copy_animal

subroutine mutate_genes(a)

    real                 :: hold
    integer              :: vals(1:8), k, i, index
    integer, allocatable :: seed(:)
    type(animal) :: a
    
    call date_and_time(values = vals)

    call random_seed(size = k)
    allocate(seed(1:k))
    seed(:) = vals(8)
    
    call random_seed(put = seed)
    call random_number(hold)
    
    index = mod(int(hold * 10), 8)
    
    a%genes(index) = a%genes(index) + mod(int(hold * 10), 8) + 1

end subroutine mutate_genes


subroutine add_random_plant(p, w, h, b)
    
    real                 :: hold
    integer              ::  i, w, h
    type(plant)          :: p(:)
        
    type(plant) :: new_plant
    logical     :: b
    
    call random_number(hold)
    
    if (b) then
        new_plant%x = mod(int(hold) * 100, w) + 1
        new_plant%y = mod(int(hold) * 30, h) + 1
    else
        new_plant%x = mod(int(hold * 10), w) + 1
        new_plant%y = mod(int(hold * 10), h) + 1
    end if
    new_plant%dead = .false.
    
    do i = 1, size(p)
        if (p(i)%dead) then
            p(i) = new_plant
            exit
        end if
    end do
    
end subroutine add_random_plant

subroutine add_animal(a, an)
    
    type(animal) :: a(:), an
    integer      :: i
    
    do i = 1, size(a)
        if (a(i)%dead) then
            a(i) = an
            exit
        end if
    end do
    
end subroutine add_animal

integer function ask_for_input()

    write(*, '(a)', advance="no") 'Days: '
    read(*, *) ask_for_input

end function ask_for_input

subroutine simulate_day(a_array, p_array)

    type(animal) :: a_array(:), hold_animals(size(a_array))
    type(plant)  :: p_array(:)
    integer :: i, width = 100, height = 30, plant_energy = 80, rep_nrg = 200
    
    call init_animal_array(hold_animals)
    
    call add_random_plant(p_array, width, height, .true.)
    call add_random_plant(p_array, width, height, .false.)
    
    do i = 1, size(a_array)
        if (.not. a_array(i)%dead) then
            call move_animal(a_array(i), width, height)
            call turn_animal(a_array(i))
            if (reproduce_animal(a_array(i), rep_nrg) .eqv. .true.) then
                hold_animals(i) = copy_animal(a_array(i))
            end if
        end if
        call is_alive(a_array(i))
    end do
    
    do i = 1, size(hold_animals)
        if (.not. hold_animals(i)%dead) then
            call add_animal(a_array, hold_animals(i))
        end if
    end do
    
end subroutine simulate_day

subroutine draw_world(a, p)

    type(animal) :: a(:)
    type(plant)  :: p(:)
    integer      :: x, y, c, width = 100, height = 30
    logical      :: is_a, is_p
    
    do x = 1, height
        write(*,*)
        write(*,'(a)', advance="no") '|'
        
        do y = 1, width
            
            is_a = .false.
            is_p = .false.
            
            do c = 1, (100 * 30)
                if (.not. a(c)%dead) then
                    if (a(c)%x == x .and. a(c)%y == y) then
                        write(*, '(a)', advance="no") 'M'
                        is_a = .true.
                    end if
                end if
                if ((.not. p(c)%dead) .and. (.not. is_a)) then
                    if (p(c)%x == x .and. p(c)%y == y) then
                        write(*, '(a)', advance="no") '*'
                        is_p = .true.
                    end if
                end if
            end do
            if ((.not. is_a) .and. (.not. is_p)) then
                write(*,'(a)', advance="no") ' '
            end if
        end do 
        write(*, '(a)', advance="no") '|'
    end do
    
    write(*,*)
end subroutine draw_world

subroutine is_alive(a)

    type(animal) :: a
    
    if (a%energy < 1) then
        a%dead = .true.
    end if
end subroutine is_alive

! try making the random number work with this - just call it in the 
! main program early on and then see what happens. . .

subroutine init_random_seed()

    INTEGER :: i, n, clock
    INTEGER, DIMENSION(:), ALLOCATABLE :: seed
    
    CALL RANDOM_SEED(size = n)
    ALLOCATE(seed(n))
    
    CALL SYSTEM_CLOCK(COUNT=clock)
    
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    CALL RANDOM_SEED(PUT = seed)
    
    DEALLOCATE(seed)
end
    
end module functions