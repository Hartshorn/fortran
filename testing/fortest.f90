program fortest

      implicit none

      real, dimension(10) :: count
100   FORMAT(f10.4)

      call random_number(count)

      write(*, 100) pack(count, count < 10)

end program fortest
