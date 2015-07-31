module functions

    use constants
    use types
    
    implicit none
    
contains
    
    subroutine push_plant(plant_array, p)
        
        type(Plant) :: p
        integer     :: i
        type(Plant), dimension(0:MAX_ARR_SIZE) :: plant_array
        
        do i = 0, MAX_ARR_SIZE
            if (.not. plant_array(i)%alive) then
                plant_array(i) = p
                exit
            end if
        end do
        
    end subroutine push_plant
    
    
    subroutine push_animal(animal_array, a)
    
        type(Animal) :: a
        integer      :: i
        type(Animal), dimension(0:MAX_ARR_SIZE) :: animal_array
        
        do i = 0, MAX_ARR_SIZE
            if (.not. animal_array(i)%alive) then
                animal_array(i) = a
                exit
            end if
        end do
        
    end subroutine push_animal
    
    
    function make_plant() result(p)
        
        type(Plant) :: p
        
        p%alive = .true.
        p%x = random(WIDTH)
        p%y = random(HEIGHT)
        
    end function make_plant
        
    
    subroutine simulate_day(as, ps)
        
        type(Animal), dimension(0:MAX_ARR_SIZE) :: as
        type(Plant),  dimension(0:MAX_ARR_SIZE) :: ps
        type(Animal), dimension(0:MAX_ARR_SIZE) :: hold_as
        integer :: i, add_count = 0
        
        call push_plant(ps, make_plant())
        call push_plant(ps, make_plant())
        
        do i = 0, MAX_ARR_SIZE
            if (as(i)%alive) then 
                call as(i)%move()
                call as(i)%turn()
                call as(i)%eat(ps)
                if (as(i)%can_reproduce()) then
                    call push_animal(hold_as, as(i)%copy_an())
                    add_count = add_count + 1
                end if
            end if
        end do
        
        do i = 0, add_count - 1
            call push_animal(as, hold_as(i))
        end do
        
        call remove_dead(as)
        
    end subroutine simulate_day
    
    recursive subroutine evolve(as, ps)
        
        type(Animal), dimension(0:MAX_ARR_SIZE) :: as
        type(Plant),  dimension(0:MAX_ARR_SIZE) :: ps
        integer :: i, days
        
        read *, days
        
        if (days .eq. 999) then
            return
        else
            do i = 1, days
            
                call simulate_day(as, ps)
            
            end do
            call draw_world(as, ps)
        end if
        
        call evolve(as, ps)
        
    end subroutine evolve
    
    subroutine remove_dead(arr)
    
        type(Animal), dimension(0:MAX_ARR_SIZE) :: arr
        integer :: i
        
        do i = 0, MAX_ARR_SIZE
        
            if (arr(i)%alive .and. arr(i)%energy < 1) then
                arr(i)%alive = .false.
            end if
            
        end do
        
    end subroutine remove_dead
    

    function random(r) result(n)
    
        integer :: r, n
        real :: rnd
        
        call random_number(rnd)
        
        n = int(rnd * r)
    
    end function random
    

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
    
    subroutine draw_world(as, ps)

        type(Animal), dimension(0:MAX_ARR_SIZE) :: as
        type(Plant),  dimension(0:MAX_ARR_SIZE) :: ps
        integer :: x, y, c
        logical :: is_a = .false., is_p = .false.
        
        do y = 0, HEIGHT
            write(*,'(a)', advance="no") "|"
            do x = 0, WIDTH
                do c = 0, MAX_ARR_SIZE
                    is_a = .false.
                    if (as(c)%x .eq. x .and. as(c)%y .eq. y) then
                        write(*,'(a)', advance="no") 'M'
                        is_a = .true.
                        exit
                    end if
                end do
                if (.not. is_a) then
                    is_p = .false.
                    do c = 0, MAX_ARR_SIZE
                        if (ps(c)%x .eq. x .and. ps(c)%y .eq. y) then
                            write(*,'(a)', advance="no") '*'
                            is_p = .true.
                            exit
                        end if
                    end do
                end if
                if (.not. is_a .and. .not. is_p) then
                    write(*,'(a)', advance="no") ' '
                end if
            end do
            write(*,*) "|"
        end do
    
    end subroutine draw_world

end module functions