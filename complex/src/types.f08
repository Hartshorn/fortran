module types

    implicit none
    private

    type, private :: Position

        integer :: x, y

    contains

        procedure :: get_x
        procedure :: set_x
        procedure :: get_y
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

    end type Agent

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

end module types
