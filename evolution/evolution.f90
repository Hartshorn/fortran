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
    
end program evolution