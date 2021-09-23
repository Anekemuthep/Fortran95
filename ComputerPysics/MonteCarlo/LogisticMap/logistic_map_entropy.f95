program logistic_map_entropy
    implicit none
    real(8), parameter :: rmin = 2.0D0
    real(8), parameter :: rmax = 4.0D0
    real(8), parameter :: xstart = 0.2D0
    integer, parameter :: RSTEPS =1000
    integer, parameter :: NHIST =10000
    integer, parameter :: NTRANS = 2000
    integer, parameter :: NSTEPS = 5000000
    real(8), parameter :: xmin=0.0D0, xmax=1.0D0
    integer :: i, ir, isum, n
    real(8) :: r, x0, x1, sum, dr, dx
    real(8) :: p(NHIST), S

    open(unit=33, file='entropy.dat')
    p = 0.0D0
    dr = (rmax-rmin)/(RSTEPS-1)
    dx = (xmax-xmin)/(NHIST-1)
    do ir=0,RSTEPS-1
        r = rmin+ir*dr
        x0 = xstart
        do i=1,NTRANS
            x1 = r * x0 * (1.0D0-x0)
            x0 = x1
        end do
        n = INT(x0/dx)+1;p(n)=p(n)+1.0D0
        do i=2,NSTEPS
            x1 = r * x0 * (1.0D0-x0)
            n = INT(x1/dx)+1
            p(n) = p(n) + 1.0D0
            x0 = x1
        end do

        p = p/NSTEPS/dx
        S = -SUM(p*log(p), MASK=p.gt.0.0D0)*dx
        write(33,*) r, S
    end do
    close(33)

    open(unit=34, file='entropy_hist.dat')
    do n=1,NHIST
        x0 = xmin + (n-1)*dx + 0.5D0*dx
        write(34,*) r, x0, p(n)
    end do
    close(34)
end program logistic_map_entropy