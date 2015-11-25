module logger

    use constants

    implicit none

contains

    subroutine log(level, message)

        Character(len=*)    :: message
        integer             :: level

        select case (level)
            case (INFO)
                    write(*,*) "       ",char(27),'[94m',message,char(27),'[0m'
                case (WARNING)
                    write(*,*) "       ",char(27),'[32m',message,char(27),'[0m'
                case (ERROR)
                    write(*,*) "       ",char(27),'[31m',message,char(27),'[0m'
                case (HEADER)
                    write(*,*) "       ",char(27),'[41m',message,char(27),'[0m'
                case default
                    write(*,*) "       ",message
        end select
    end subroutine log
end module logger
