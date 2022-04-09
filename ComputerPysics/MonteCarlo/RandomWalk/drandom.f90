real(8) function drandom()
    implicit none
    integer, parameter :: a = 16807
    integer, parameter :: m = 2147483647
    integer, parameter :: q = 127773
    integer, parameter :: r = 2836
    real(8), parameter :: f = (1.0D0/m)
    integer :: p
    integer :: seed
    real(8) :: dr
    common /randoms/ seed
 101 continue
    p = seed/q
    seed = a*(seed-q*p)-r*p
    if(seed .lt. 0) seed = seed + m
    dr = f*seed
    if(dr .le. 0.0D0 .or. dr .ge. 1.0D0) goto 101
    drandom = dr
end function drandom