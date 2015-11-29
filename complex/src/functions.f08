module functions

    use constants
    use types

    implicit none

contains

    subroutine init_world()

        call init_random_seed()
        call set_w_h(GLOBAL_WIDTH, GLOBAL_HEIGHT)
        call generate_vegitation()

    end subroutine init_world

    subroutine set_w_h(width, height)

        integer, intent(inout)  :: width, height
        character(len=32)       :: width_c, height_c

        call getArg(1, width_c)
        call getArg(2, height_c)

        read(width_c,  '(i5)') width
        read(height_c, '(i5)') height

        GLOBAL_MAX = width * height

        allocate(GLOBAL_WORLD(0:GLOBAL_MAX))
        allocate(PLANTS(0:GLOBAL_MAX))

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

    subroutine add_agent(x, y)
        ! add logic (and parameter) for adding different types
        integer :: x, y
        type(Explorer) :: new_agent

        call new_agent%init(x, y)

        if(AGENT_COUNT < GLOBAL_WIDTH * GLOBAL_HEIGHT) then
            AGENT_COUNT = AGENT_COUNT + 1
            GLOBAL_WORLD(AGENT_COUNT) = new_agent
        end if

    end subroutine add_agent

    subroutine add_plant(x, y)
        integer, intent(in) :: x, y
        if(PLANT_COUNT < GLOBAL_WIDTH * GLOBAL_HEIGHT) then
            PLANT_COUNT = PLANT_COUNT + 1
            PLANTS(y * GLOBAL_WIDTH + x) = .true.
        end if
    end subroutine add_plant

    subroutine generate_vegitation()
        integer :: factor, x, y, i

        factor = int(GLOBAL_MAX * PLANT_COVERAGE)

        do i=0, factor
            x = random(GLOBAL_WIDTH)
            y = random(GLOBAL_HEIGHT)
            call add_plant(x, y)
        end do

    end subroutine generate_vegitation

    subroutine event_loop()

        integer :: x, y, i

        do while (.not.(x .eq. 999))

            call execute_command_line('clear')

            do i=0, AGENT_COUNT
                call write_at("!", GLOBAL_WORLD(i)%get_x(), GLOBAL_WORLD(i)%get_y())
                call GLOBAL_WORLD(i)%forage(GLOBAL_WIDTH, PLANTS)
                ! call GLOBAL_WORLD(i)%show()
            end do

            read(*,*) x, y

            if (x .lt. GLOBAL_WIDTH .and. y .lt. GLOBAL_HEIGHT) then

                call add_agent(x, y)

            end if
        end do

    end subroutine event_loop

    function random(r) result(n)

        integer :: r, n
        real :: rnd

        call random_number(rnd)

        n = int(rnd * r)

    end function random


    subroutine init_random_seed()

        integer :: t, sz, clock
        integer, dimension(:), allocatable :: seed

        call random_seed(size = sz)
        allocate(seed(sz))
        call system_clock(count = clock)
        seed = clock + 37 * (/(t - 1, t = 1, sz)/)
        call random_seed(put = seed)
        deallocate(seed)

    end subroutine init_random_seed

end module functions
