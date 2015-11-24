module types

    implicit none
    private

    type, private :: Position

        integer :: x, y

    contains
        
        procedure :: get_x   => get_x_value
        procedure :: set_x   => set_x_value
        procedure :: get_y   => get_y_value
        procedure :: set_y   => set_y_value
        procedure :: get_loc => get_x_and_y

    end type Position

    type, public :: Agent

        logical :: active = .false.
        type(Position) :: location

    contains

        procedure :: init => create_with_position

    end type Agent

contains

    pure function get_x_value(this) result(x)
        class(Position), intent(in) :: this
        integer :: x
        x = this%x
    end function get_x_value

    subroutine set_x_value(this, x)
        class(Position), intent(inout) :: this
        integer :: x
        this%x = x
    end subroutine set_x_value

    pure function get_y_value(this) result(y)
        class(Position), intent(in) :: this
        integer :: y
        y = this%y
    end function get_y_value

    subroutine set_y_value(this, y)
        class(Position), intent(inout) :: this
        integer :: y
        this%y = y
    end subroutine set_y_value

    subroutine get_x_and_y(this, x, y)
        class(Position), intent(in) :: this
        integer, intent(inout) :: x, y
        x = this%x
        y = this%y
    end subroutine get_x_and_y

    subroutine create_with_position(this, x, y)

        class(Agent), intent(inout) :: this
        integer, intent(in)         :: x, y
        type(Position)              :: p

        call p%set_x(x)
        call p%set_y(y)

        this%location = p

    end subroutine create_with_position

end module types
