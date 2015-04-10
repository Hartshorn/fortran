program evolve
    
    use types
    use functions
    
    implicit none
    
        
    integer :: i
    type(plant)  :: p_array(100 * 30)
    type(animal) :: a, a_array(100 * 30)
    
    integer              :: vals(1:8), k
    integer, allocatable :: seed(:)
        
    type(plant) :: new_plant
    logical     :: b
    
    call date_and_time(values = vals)

    call random_seed(size = k)
    allocate(seed(1:k))
    seed(:) = vals(8)
    
    call random_seed(put = seed)


    call init_plant_array(p_array)
    call init_animal_array(a_array)
    
    call init_animal(a)
    call add_animal(a_array, a)
    
    do i = 1, 5
        call simulate_day(a_array, p_array)
    end do
    
!    do i = 1, size(p_array)
!        if (.not. p_array(i)%dead) then
!            call show_plant(p_array(i), i)
!        end if
!    end do
!    
!    do i = 1, size(a_array)
!        if (.not. a_array(i)%dead) then
!            call show_animal(a_array(i), i)
!        end if
!    end do
!    user_number = ask_for_input()
    
    call draw_world(a_array, p_array)
    
end program evolve