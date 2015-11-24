program complex

    use functions
    use types

    implicit none

    integer :: x, y

    call init_world()

    call GLOBAL_WORLD(0)%location%get_loc(x, y)

    call write_at("!", x, y)


end program complex
