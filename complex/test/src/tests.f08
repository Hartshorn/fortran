module tests

    use types
    use constants
    use logger

    implicit none

    integer :: INIT_X = 1, INIT_Y = 2, CHG_X = 3, CHG_Y = 4


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
        type(Agent) :: a

        call log(INFO, "Test Case Begin: test_agent_init")
        call log(INFO, "    Before Initialization:")

        if(.not. a%get_active()) then
            call log(WARNING, "Passed")
            write(*,*) "            a%get_active(): ", a%get_active()
        else
            call log(ERROR, "FAILED!!")
            write(*,*) "            a%get_active(): ", a%get_active()
        end if

        call a%init(INIT_X, INIT_Y)

        call log(INFO, "    After Initialization: Agent%init")

        if(a%get_active()) then
            call log(WARNING, "Passed")
            write(*,*) "            a%get_active(): ", a%get_active()
        else
            call log(ERROR, "FAILED!!")
            write(*,*) "            a%get_active(): ", a%get_active()
        end if

        if(assert_eq(a%get_x(), INIT_X)) then
            call log(WARNING, "Passed")
            write(*,*) "            a%get_x(): ", a%get_x()
        else
            call log(ERROR, "FAILED!!")
            write(*,*) "            a%get_x(): ", a%get_x()
        end if

        if(assert_eq(a%get_y(), INIT_Y)) then
            call log(WARNING, "Passed")
            write(*,*) "            a%get_y(): ", a%get_y()
        else
            call log(ERROR, "FAILED!!")
            write(*,*) "            a%get_y(): ", a%get_y()
        end if

        call log(INFO, "Test Case End: test_agent_init")
    end subroutine test_agent_init

    subroutine test_agent_move(a)
        type(Agent) :: a

        call log(INFO, "Test Case Begin: test_agent_move")

        if(a%get_active()) then
            call log(WARNING, "Passed")
            write(*,*) "            a%get_active(): ", a%get_active()
            write(*,*) "            a%get_x(): ", a%get_x()
            write(*,*) "            a%get_y(): ", a%get_y()
        else
            call log(ERROR, "FAILED!!")
            write(*,*) "            a%get_active(): ", a%get_active()
            write(*,*) "            a%get_x(): ", a%get_x()
            write(*,*) "            a%get_y(): ", a%get_y()
        end if

        call log(INFO, "    After: Agent%move")

        call a%move(CHG_X, CHG_Y)

        if(assert_eq(a%get_x(), INIT_X + CHG_X)) then
            call log(WARNING, "Passed")
            write(*,*) "            a%get_x(): ", a%get_x()
        else
            call log(ERROR, "FAILED!!")
            write(*,*) "            a%get_x(): ", a%get_x()
        end if

        if(assert_eq(a%get_y(), INIT_Y + CHG_Y)) then
            call log(WARNING, "Passed")
            write(*,*) "            a%get_y(): ", a%get_y()
        else
            call log(ERROR, "FAILED!!")
            write(*,*) "            a%get_y(): ", a%get_y()
        end if

        call log(INFO, "Test Case End: test_agent_move")

    end subroutine test_agent_move

end module tests

program test_runner

    use types
    use constants
    use functions
    use tests
    use logger

    implicit none

    type(Agent) :: test_agent
    call log(HEADER, "        ....::::BEGIN TEST RUNNER::::....        ")

    call test_agent_init(test_agent)
    call test_agent_move(test_agent)

    call log(HEADER, "        ....:::::END TEST RUNNER:::::....        ")
end program test_runner
