module functions

    use constants
    use types

    implicit none

contains

    subroutine init_world()

        type(Agent) :: a

        call set_w_h(GLOBAL_WIDTH, GLOBAL_HEIGHT)
        call a%init(10, 20)

        GLOBAL_WORLD(a%get_x() * a%get_y()) = a

    end subroutine init_world

    subroutine set_w_h(width, height)

        integer, intent(inout)  :: width, height
        character(len=32)       :: width_c, height_c

        call getArg(1, width_c)
        call getArg(2, height_c)

        read(width_c, '(i5)') width
        read(height_c, '(i5)') height

        allocate(GLOBAL_WORLD(0:width*height))

    end subroutine set_w_h

    subroutine move_to(x, y)

        integer, intent(in)         :: x, y
        character(len=20)           :: ln
        character(len=5)            :: x_str, y_str
        character(:), allocatable   :: out_ln

        ln = "tput cup "
        write(x_str, '(I5)') x
        write(y_str, '(I5)') y

        allocate(character(len=26) :: out_ln)

        out_ln = ln//y_str//x_str

        call execute_command_line(out_ln)

    end subroutine move_to

    subroutine write_at(c, x, y)

        integer, intent(in), optional   :: x, y
        integer                         :: dx, dy
        character, intent(in)           :: c

        if (present(x) .and. present(y)) then
            dx = mod((GLOBAL_WIDTH  / 2) + x, GLOBAL_WIDTH  / 2)
            dy = mod((GLOBAL_HEIGHT / 2) + y, GLOBAL_HEIGHT / 2)
            call move_to(dx, dy)
        else
            call move_to((GLOBAL_WIDTH / 2), (GLOBAL_HEIGHT / 2))
        end if

        write(*,'(a)') c
        call move_to(0, GLOBAL_HEIGHT - 2)

    end subroutine write_at

    subroutine event_loop()
        integer :: x, y, i
        do while (.not.(x .eq. 999))
            call execute_command_line('clear')
            do i=0, GLOBAL_WIDTH * GLOBAL_HEIGHT
                if (GLOBAL_WORLD(i)%active .eqv. .true.) then
                    call write_at("!", GLOBAL_WORLD(i)%get_x(), GLOBAL_WORLD(i)%get_y())
                end if
            end do
            read(*,*) x, y
            if (x .lt. GLOBAL_WIDTH .and. y .lt. GLOBAL_HEIGHT) then
                call GLOBAL_WORLD(x * y)%set_active(.true.)
                call GLOBAL_WORLD(x * y)%set_x(x)
                call GLOBAL_WORLD(x * y)%set_y(y)
            end if
        end do
    end subroutine event_loop

end module functions
