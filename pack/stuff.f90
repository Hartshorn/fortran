module stuff

implicit none

contains

subroutine show(data)

100     FORMAT(f17.4)
200     FORMAT(a7, f10.4)
300     FORMAT(a7, i6)

    real :: data(:)
    
    write(*, 300) 'length', size(data)
    write(*, 200) 'sum', sum(data)
    write(*, 200) 'mean', sum(data) / size(data)
    
    print *, 'elements above the mean'
    write(*, 100) pack( data, data > mean(data) )
    
    print *, 'mean of elements above the mean'
    write(*, 100) mean( pack( data, data > mean(data) ) )
    
end subroutine show

real function mean(data)
    
    real :: data(:)
    
    mean = sum(data) / size(data)

end function mean

end module stuff