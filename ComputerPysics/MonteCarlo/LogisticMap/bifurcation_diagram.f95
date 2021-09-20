!=========================================
! Bifurcation Diagram of the Logistic Map
!=========================================
program bifurcation_map
    implicit none
    real(8), parameter :: rmin = 2.5D0
    real(8), parameter :: rmax = 4.0D0
    integer, parameter :: NTRANS = 500 ! number of discarded steps
    integer, parameter :: NSTEPS = 100 ! number of recorded steps
    integer, parameter :: RSTEPS = 2000 ! number of values of t
    integer :: i
    real(8) :: r, dr, x0, x1
!----- Initialize
    open(unit=33, file='bif.dat')
    dr = (rmax-rmin) / RSTEPS ! Increment in r
!----- Calculate:
    r = rmin
    do while (r .le. rmax)
        x0 = 0.5D0
!----- Transient steps: skip
        do i=1,NTRANS
            x1 = r * x0 * (1.0D0-x0)
            x0 = x1
        end do
        do i=1,NSTEPS
            x1 = r * x0 * (1.0D0-x0)
            write(33,*) r, x1
            x0 = x1
        end do
        r = r + dr
    end do
    close(33)
end program  bifurcation_map