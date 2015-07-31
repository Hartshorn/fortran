program evolution

    use functions
    use constants
    use types

    implicit none
    
    

    type(Animal), dimension(0:MAX_ARR_SIZE) :: Animals
    type(Plant),  dimension(0:MAX_ARR_SIZE) :: Plants

    type(Animal) :: a
    
    
    call init_random_seed()
    
    call a%init()
    
    Animals(0) = a
    
    call evolve(Animals, Plants)
    
    write(*,*) "Animal count: ", count_animals(Animals)
    write(*,*) "Plant count : ", count_plants(Plants)
    
    !call show_animals(Animals)
    
    
contains

    pure function count_animals(a_arr) result(n)
        
        type(Animal), intent(in), dimension(0:MAX_ARR_SIZE) :: a_arr
        integer :: i, n
        
        do i = 0, MAX_ARR_SIZE
            if (a_arr(i)%alive) then
                n = n + 1
            end if
        end do
    end function count_animals
    
    pure function count_plants(p_arr) result(n)
        
        type(Plant), intent(in), dimension(0:MAX_ARR_SIZE) :: p_arr
        integer :: i, n
        
        do i = 0, MAX_ARR_SIZE
            if (p_arr(i)%alive) then
                n = n + 1
            end if
        end do
    end function count_plants
    
    
    subroutine show_animals(as)
    
        type(Animal), dimension(0:MAX_ARR_SIZE) :: as
        integer :: i
        
        do i = 0, MAX_ARR_SIZE
            if (as(i)%alive) then
                call as(i)%show()
            end if
        end do
    end subroutine show_animals

end program evolution