module types

    implicit none
    private

    type, public :: value
    
        logical :: has_value = .false.
        integer :: how_much  = 0
        
    contains
    
        procedure :: print => show_value
        procedure :: init  => setup_values
        
    end type value

contains

    subroutine show_value(this)
        
        100     FORMAT(a,i3)
        200     FORMAT(a,l2)

        class(value), intent(in) :: this
        
        write(*,200) 'Has value: ', this%has_value
        write(*,100) 'How much: ' , this%how_much
    
    end subroutine show_value
    
    subroutine setup_values(this, n, m)

        class(value), intent(inout) :: this
        
        integer :: n
        logical :: m
        
        this%has_value = m
        this%how_much = n

    end subroutine setup_values
        
end module types