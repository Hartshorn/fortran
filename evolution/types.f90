module types

    implicit none
    private

    
    type, public :: Animal
    
        logical                 :: alive = .false.
        integer                 :: x, y, energy = 1000, direction = 0
        integer, dimension(0:8) :: genes
        
    contains
    
        procedure :: show   => describe_animal
        procedure :: copy   => get_new_animal
        procedure :: cpy_gn => copy_genes
        
    end type Animal
    
    
    type, public :: Plant
        
        logical :: alive = .false.
        integer :: x, y
    
    contains
    
        procedure :: show => describe_plant
    
    end type Plant
    
    
contains
    
    subroutine describe_animal(this)
        
        100 FORMAT(a,i3,a,i3,a)
        200 FORMAT(a,i4)
        300 FORMAT(a,i1)
        400 FORMAT(a)
        
        class(Animal) :: this
        
        if (this%alive) then
            write(*,100) "Location: (", this%x, ",", this%y, " )"
            write(*,200) "Energy:  ", this%energy
            write(*,300) "Direction: ", this%direction
        else
            write(*,400) "This animal is dead!"
        end if
        
    end subroutine describe_animal
    
    
    subroutine describe_plant(this)
        
        100 FORMAT(a,i3,a,i3,a)
        400 FORMAT(a)
        
        class(Plant) :: this
        
        if (this%alive) then
            write(*,100) "Location: (", this%x, ",", this%y, " )"
        else
            write(*,400) "This plant is dead!"
        end if
        
    end subroutine describe_plant
    
    
    pure function get_new_animal(this) result(new_a)
    
        class(Animal), intent(in) :: this
        type(Animal) :: new_a
        
        new_a%alive     = .true.
        new_a%x         = this%x
        new_a%y         = this%y
        new_a%energy    = this%energy
        new_a%genes     = this%cpy_gn()
        
    end function get_new_animal
    
    
    pure function copy_genes(this) result(c_genes)
        
        class(Animal), intent(in)   :: this
        integer, dimension(0:8)     :: c_genes
        integer                     :: i
        
        do i = 0, 8
            c_genes(i) = this%genes(i)
        end do
        
    end function copy_genes

end module types