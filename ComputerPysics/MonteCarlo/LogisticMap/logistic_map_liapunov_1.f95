!=====================================================
! Discrete Logistic Map:
! Two trajectories with close initial conditions.
!=====================================================
program logistic_map_liapunov_1
    implicit none
    integer :: NSTEPS, i
    real(8) :: r,x0,x1,x0t,x1t, epsilon

!------ Input
    print *, '# Enter NSTEPS, r, x0, epsilon'
    read *, NSTEPS, r, x0, epsilon
    print *, '# NSTEPS  = ', NSTEPS
    print *, '# r       = ', r
    print *, '# x0      = ', x0
    print *, '# epsilon =', epsilon

    x0t = x0 + epsilon
!----- Initialize
    open(unit=33, file='lia.dat')
    write(33,*) 1, x0, x0t, ABS(x0t-x0)/epsilon
!----- Calculate
    do i=1,NSTEPS
        x1 = r * x0 * (1.0D0-x0)
        x1t = r * x0t * (1.0D0-x0t)
        write(33, *) i, x1, x1t, ABS(x1t-x1)/epsilon
        x0 = x1; x0t = x1t
    end do
    close(33)
end program logistic_map_liapunov_1