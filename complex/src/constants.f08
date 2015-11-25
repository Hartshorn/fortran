module constants

    use types

    implicit none

        type(Agent), dimension(:), allocatable :: GLOBAL_WORLD

        integer :: GLOBAL_WIDTH         = 0
        integer :: GLOBAL_HEIGHT        = 0

        integer, parameter :: INFO      = 1
        integer, parameter :: WARNING   = 2
        integer, parameter :: ERROR     = 3
        integer, parameter :: HEADER    = 4


end module constants
