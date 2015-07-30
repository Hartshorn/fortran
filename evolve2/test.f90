program test

    implicit none

    integer :: somenumber = 10, someothernumber = 15, newnumber

!    newnumber = getnewnumber(somenumber, someothernumber)

    write(*, '(i3)') somenumber

end program test

function getnewnumber(sn, son)

    integer :: sn, son

    getnewnumber = sn * son

end function getnewnumber
