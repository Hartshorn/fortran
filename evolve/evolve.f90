program evolve

    use types
    use functions

    implicit none


    integer :: i, user_number
    type(plant)  :: p_array(100 * 30)
    type(animal) :: a, a_array(100 * 30)

    call init_random_seed()


    call init_plant_array(p_array)
    call init_animal_array(a_array)

    call init_animal(a)
    call add_animal(a_array, a)

    do
        user_number = ask_for_input()
        do i = 1, user_number
            call simulate_day(a_array, p_array)
        end do
        call draw_world(a_array, p_array)
    end do

   do i = 1, size(p_array)
       if (.not. p_array(i)%dead) then
           call show_plant(p_array(i), i)
       end if
   end do
!
!    do i = 1, size(a_array)
!        if (.not. a_array(i)%dead) then
!            call show_animal(a_array(i), i)
!        end if
!    end do



end program evolve
