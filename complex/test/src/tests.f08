module tests

    use types
    use constants
    use logger

    implicit none

    integer :: INIT_X = 1, INIT_Y = 2, CHG_X = 3, CHG_Y = 4
    integer :: INIT_ENERGY = 1000, CHG_ENERGY = 1010


contains

    pure integer function f1(x) result(r)
        integer, intent(in) :: x

        r = x + 10
    end function f1

    integer function f3(f_ptr, x) result(r)
        integer, intent(in) :: x
        integer, external   :: f_ptr

        r = f_ptr(x)
    end function

    pure logical function assert_eq(x, y) result(r)
        integer, intent(in) :: x, y
            r = x .eq. y
    end function assert_eq

    subroutine test_agent_init(a)
        type(Explorer) :: a

        call log(INFO, "Test Case Begin: test_agent_init")
        call log(INFO, "    Before Initialization:")

        if(.not. a%get_active()) then
            call log(WARNING, "Passed")
            call a%show()
        else
            call log(ERROR, "FAILED!!")
            call a%show()
        end if

        call a%init(INIT_X, INIT_Y)

        call log(INFO, "    After Initialization: Agent%init")

        if(a%get_active()) then
            call log(WARNING, "Passed")
            call a%show()
        else
            call log(ERROR, "FAILED!!")
            call a%show()
        end if

        if(assert_eq(a%get_x(), INIT_X)) then
            call log(WARNING, "Passed")
            call a%show()
        else
            call log(ERROR, "FAILED!!")
            call a%show()
        end if

        if(assert_eq(a%get_y(), INIT_Y)) then
            call log(WARNING, "Passed")
            call a%show()
        else
            call log(ERROR, "FAILED!!")
            call a%show()
        end if

        call log(INFO, "Test Case End: test_agent_init")
    end subroutine test_agent_init

    subroutine test_agent_move(a)
        type(Explorer) :: a

        call log(INFO, "Test Case Begin: test_agent_move")

        if(a%get_active()) then
            call log(WARNING, "Passed")
            call a%show()
        else
            call log(ERROR, "FAILED!!")
            call a%show()
        end if

        call log(INFO, "    After: Agent%move")

        call a%move(CHG_X, CHG_Y)

        if(assert_eq(a%get_x(), INIT_X + CHG_X)) then
            call log(WARNING, "Passed")
            call a%show()
        else
            call log(ERROR, "FAILED!!")
            call a%show()
        end if

        if(assert_eq(a%get_y(), INIT_Y + CHG_Y)) then
            call log(WARNING, "Passed")
            call a%show()
        else
            call log(ERROR, "FAILED!!")
            call a%show()
        end if

        call log(INFO, "Test Case End: test_agent_move")

    end subroutine test_agent_move

    subroutine test_agent_forage(a)
        type(Explorer) :: a

        call log(INFO, "Test Case Begin: test_agent_forage")

        PLANTS(a%index(GLOBAL_WIDTH)) = .true.

        call log(INFO, "    Before forage")
        if(assert_eq(a%energy, INIT_ENERGY)) then
            call log(WARNING, "Passed")
            call a%show()
        else
            call log(ERROR, "FAILED!!")
            call a%show()
        end if

        call a%forage(GLOBAL_WIDTH, PLANTS)

        call log(INFO, "    After forage")
        if(assert_eq(a%energy, CHG_ENERGY)) then
            call log(WARNING, "Passed")
            call a%show()
        else
            call log(ERROR, "FAILED!!")
            call a%show()
        end if

        call log(INFO, "Test Case End: test_agent_forage")

    end subroutine test_agent_forage

end module tests

program test_runner

    use types
    use constants
    use functions
    use tests
    use logger

    implicit none

    type(Explorer) :: test_agent
    call init_world()

    call log(HEADER, "        ....::::BEGIN TEST RUNNER::::....        ")

    call test_agent_init(test_agent)
    call test_agent_move(test_agent)
    call test_agent_forage(test_agent)

    call log(HEADER, "        ....:::::END TEST RUNNER:::::....        ")
end program test_runner
