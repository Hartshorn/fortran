module types

    implicit none
    private

    integer, parameter, public :: EXPLORER_INIT_ENERGY = 1000
    integer, parameter, public :: PLANT_CONSUMPTION_ENERGY = 10

    type, private :: Position

        integer :: x, y

    contains

        procedure :: get_x
        procedure :: get_y
        procedure :: set_x
        procedure :: set_y

    end type Position

    type, public :: Agent

        logical :: active = .false.
        type(Position) :: location

    contains

        procedure :: get_x      => get_pos_x
        procedure :: get_y      => get_pos_y
        procedure :: set_x      => set_pos_x
        procedure :: set_y      => set_pos_y
        procedure :: get_active
        procedure :: set_active
        procedure :: init       => create_with_position
        procedure :: move       => move_by_xy_factor
        procedure :: index      => calculate_world_index

    end type Agent

    type, public, extends(agent):: Explorer

        integer :: energy = EXPLORER_INIT_ENERGY

    contains

        procedure :: forage => search_for_food
        procedure :: get_energy
        procedure :: set_energy
        procedure :: show => debug_show_agent

    end type Explorer


contains

    pure integer function get_x(this) result(x)
        class(Position), intent(in) :: this
        x = this%x
    end function get_x

    subroutine set_x(this, x)
        class(Position), intent(inout) :: this
        integer :: x
        this%x = x
    end subroutine set_x

    pure integer function get_y(this) result(y)
        class(Position), intent(in) :: this
        y = this%y
    end function get_y

    subroutine set_y(this, y)
        class(Position), intent(inout) :: this
        integer :: y
        this%y = y
    end subroutine set_y

    pure integer function get_pos_x(this) result(x)
        class(Agent), intent(in) :: this
        type(Position) :: pos
        pos = this%location
        x = pos%get_x()
    end function get_pos_x

    subroutine set_pos_x(this, x)
        Class(Agent), intent(inout) :: this
        integer, intent(in) :: x
        call this%location%set_x(x)
    end subroutine set_pos_x

    pure integer function get_pos_y(this) result(y)
        class(Agent), intent(in) :: this
        type(Position) :: pos
        pos = this%location
        y = pos%get_y()
    end function get_pos_y

    subroutine set_pos_y(this, y)
        Class(Agent), intent(inout) :: this
        integer, intent(in) :: y
        call this%location%set_y(y)
    end subroutine set_pos_y

    pure logical function get_active(this) result(act)
        class(Agent), intent(in) :: this
        act = this%active
    end function get_active

    subroutine set_active(this, act)
        class(Agent), intent(inout) :: this
        logical, intent(in)         :: act
        this%active = act
    end subroutine set_active

    subroutine create_with_position(this, x, y)

        class(Agent), intent(inout) :: this
        integer, intent(in)         :: x, y
        type(Position)              :: p

        call p%set_x(x)
        call p%set_y(y)

        this%location = p
        this%active = .true.

    end subroutine create_with_position

    subroutine move_by_xy_factor(this, x, y)
        integer, intent(in) :: x, y
        Class(Agent) :: this
        call this%set_x(this%get_x() + x)
        call this%set_y(this%get_y() + y)
    end subroutine move_by_xy_factor

    pure integer function calculate_world_index(this, g_width) result(idx)
        Class(Agent), intent(in) :: this
        integer, intent(in) :: g_width

        idx = this%get_y() * g_width + this%get_x()
    end function calculate_world_index

    subroutine search_for_food(this, g_width, plants)
        Class(Explorer), intent(inout)  :: this
        integer, intent(in)             :: g_width
        logical, dimension(:)           :: plants

        if(plants(this%index(g_width))) then
            this%energy = this%energy + PLANT_CONSUMPTION_ENERGY
            plants(this%index(g_width)) = .false.
        end if
    end subroutine search_for_food

    pure integer function get_energy(this) result(nrg)
        Class(Explorer), intent(in) :: this
        nrg = this%energy
    end function get_energy

    subroutine set_energy(this, nrg)
        Class(Explorer), intent(inout) :: this
        integer, intent(in) :: nrg
        this%energy = nrg
    end subroutine set_energy

    subroutine debug_show_agent(this)
        Class(Explorer), intent(in) :: this
        write(*,100) "x:",      this%get_x(),       &
                     "y:",      this%get_y(),       &
                     "energy:", this%get_energy(),  &
                     "active:", this%get_active()
        100 FORMAT(T12,a3,i2,a3,i2,a8,i5,a8,l2)
    end subroutine debug_show_agent

end module types
