program nearest_neighbor

  use neighbor_func

  implicit none

  integer :: i

  write(*, '(f10.4)') cdiff( sort( (/ (mod(i * sqrt(2.0), 1.0), i = 1, 20) /) ) )

end program nearest_neighbor
