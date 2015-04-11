program evolve
    
    use types
    use functions

    implicit none
    
    integer :: width = 100, height = 30, is, js, days, c
    type(organism) :: world(0:100, 0:30)
    logical :: rep
    
    call init_random_seed()
    
    call add_animal(world, width / 2, height / 2)
    
    do
        days = get_days()
        
        do c = 1, days
            call simulate_day(world, width, height)
        end do
        
        call draw_world(world, width, height)
        
    end do
    
end program evolve