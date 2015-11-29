module constants

    use types

    implicit none

        type(Explorer), dimension(:), allocatable  :: GLOBAL_WORLD
        logical, dimension(:), allocatable :: PLANTS

        integer :: GLOBAL_WIDTH
        integer :: GLOBAL_HEIGHT
        integer :: GLOBAL_MAX

        integer, parameter :: INFO      = 1
        integer, parameter :: WARNING   = 2
        integer, parameter :: ERROR     = 3
        integer, parameter :: HEADER    = 4

        integer :: AGENT_COUNT          = -1
        integer :: PLANT_COUNT          = -1

        real :: PLANT_COVERAGE          = 0.5


end module constants
