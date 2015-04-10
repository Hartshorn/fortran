module neighbor_func

implicit none

contains

  function sort( array )
    real, dimension(:)           :: array
    real, dimension(size(array)) :: sort

    real :: temp
    integer :: i, j
    integer, dimension(1) :: pos

    sort = array
    do i = 1, size(sort)
      pos = minloc( sort(i:) )
      j = i + pos(1) - 1
      temp = sort(j)
      sort(j) = sort(i)
      sort(i) = temp
    enddo
  end function sort

  function cdiff( array )
    real, dimension(:) :: array
    real, dimension(size(array)) :: cdiff

    cdiff = abs( array - cshift(array, 1) )
    cdiff = min( cdiff, 1.0 - cdiff )
  end function cdiff

end module neighbor_func
