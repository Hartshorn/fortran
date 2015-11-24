module functions

    use constants
    use types

    implicit none

contains

    subroutine init_world()
        type(Agent) :: a

        call set_w_h(GLOBAL_WIDTH, GLOBAL_HEIGHT)
        call a%init(2, 3)

        GLOBAL_WORLD(0) = a

    end subroutine init_world

    subroutine set_w_h(width, height)

        integer, intent(inout)  :: width, height
        character(len=32)       :: width_c, height_c

        call getArg(1, width_c)
        call getArg(2, height_c)

        read(width_c, '(i5)') width
        read(height_c, '(i5)') height

    end subroutine set_w_h

    subroutine move_to(x, y)

        integer, intent(in)         :: x, y
        character(len=20)           :: ln
        character(len=3)            :: x_str, y_str
        character(:), allocatable   :: out_ln

        ln = "tput cup "
        write(x_str, '(I3)') x
        write(y_str, '(I3)') y

        allocate(character(len=26) :: out_ln)

        out_ln = ln//y_str//x_str

        call execute_command_line('clear')
        call execute_command_line(out_ln)

    end subroutine move_to

    subroutine write_at(c, x, y)

        integer, intent(in), optional   :: x, y
        character, intent(in)           :: c

        if (present(x) .and. present(y)) then
            call move_to((GLOBAL_WIDTH / 2) + x, (GLOBAL_HEIGHT / 2) + y)
        else
            call move_to((GLOBAL_WIDTH / 2), (GLOBAL_HEIGHT / 2))
        end if

        write(*,'(a)') c

    end subroutine write_at

end module functions
