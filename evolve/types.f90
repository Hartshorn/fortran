module types

    type animal
        Integer :: x, y, dir, energy, genes(8)
        Logical :: dead
    end type animal
    
    type plant
        Integer :: x, y
        Logical :: dead
    end type plant
    
end module types