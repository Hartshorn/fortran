program packTest

    use stuff

    implicit none;
    
    real                 :: data(1:10)
    integer              :: vals(1:8), k
    integer, allocatable :: seed(:)
    
    call date_and_time(values = vals)

    call random_seed(size = k)
    allocate(seed(1:k))
    seed(:) = vals(8)
    
    call random_seed(put = seed)
    call random_number(data)

    call show(data)

end program packTest