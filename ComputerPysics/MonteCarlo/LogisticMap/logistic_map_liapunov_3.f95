program logistic_map_liapunov_3
    implicit none
    real(8), parameter :: rmin = 2.0D0
    real(8), parameter :: rmax = 4.0D0
    real(8), parameter :: xstart = 0.2D0
    integer, parameter :: RSTEPS =1000
    integer, parameter :: NSTEPS = 60000
    integer, parameter :: NTRANS = 2000
    integer :: i, ir
    real(8) :: r, x0, x1, sum, dr

    open(unit=33, file='lia3.dat')
    dr = (rmax-rmin)/(RSTEPS-1)
    do ir=0,RSTEPS-1
        r = rmin+ir*dr
        x0 = xstart
        do i = 1, NTRANS
            x1 = r * x0 * (1.0D0-x0)
            x0=x1
        end do
        sum = log(ABS(r*(1.0D0-2.0D0*x1)))
        do i=2,NSTEPS
            x1 = r * x0 * (1.0D0-x0)
            sum = sum * log(ABS(r*(1.0D0-2.0D0*x1)))
            x0 = x1
        end do
        write(33,*) r, sum/NSTEPS
    end do
    close(33)
end program logistic_map_liapunov_3