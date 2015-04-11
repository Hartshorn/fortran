module types

    implicit none
    
    integer :: i
    
    type organism
        logical :: has_animal = .false., has_plant = .false.
        integer :: energy = 1000, dir = 0, genes(8) 
        
!       add a number for the amount at one space - there could be more than one
    
    end type organism

contains

subroutine show_organism(a)

    type(organism) :: a
    
    write(*,*) 'Animal?', a%has_animal
    write(*,*) 'Plant?', a%has_plant
    write(*,*) 'Energy?', a%energy
    write(*,*) 'Direction?', a%dir
    write(*,*) 'Genes?', a%genes
    
end subroutine show_organism
    
end module types