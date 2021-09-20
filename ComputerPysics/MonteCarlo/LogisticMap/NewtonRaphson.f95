program NewtonRaphson
    implicit none
    real(8), parameter :: rho = 15.0D0
    real(8), parameter :: eps = 1D-6
    integer, parameter :: NMAX = 1000
    real(8) :: x0, x1, err, g, gp
    integer :: i
    print *, 'Enter x0: '
    read *, x0
    err = 1.0D0
    print *, 'iter                   x                                error  '
    print *, '_______________________________________________________________'
    print *, 0, x0, err
    do i=1, NMAX
!--------- Value of the function g(x) :
        g  = x0 * tan(x0)-sqrt(rho*rho-x0*x0)
!--------- Value of the derivative g'(x):
        gp = x0 /sqrt(rho*rho-x0*x0) + x0/(cos(x0)**2)+tan(x0)
        x1 = x0 - g/gp
        err = ABS(x1-x0)
        print *, i, x1, err
        if(err .lt. eps) exit
        x0 = x1
    end do
end program NewtonRaphson