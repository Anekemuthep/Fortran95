program logistic_map_liapunov_2
    implicit none
    integer :: NTRANS, NSTEPS, i
    real(8) :: r, x0, x1, sum

!----- Input:
    print *, '# Enter NTRANS, NSTEPS, r, x0: '
    read *, NTRANS, NSTEPS, r, x0
    print *, '# NTRANS =  ', NTRANS
    print *, '# NSTEPS = ', NSTEPS
    print *, '# r      = ', r
    print *, '# x0     = ', x0

    do i=1,NTRANS
        x1 = r * x0 * (1.0D0-x0)
        x0 = x1
    end do
    sum = log(ABS(r*(1.0D0-2.0D0*x0)))
!---- Initialize:
    open(unit=33, file='lia2.dat')
    write(33,*) 1, x0, sum
!---- Calculate:
    do i = 2, NSTEPS
        x1 = r * x0 * (1.0D0-x0)
        sum = sum + log(ABS(r*(1.0D0-2.0D0*x1)))
        write(33,*) i, x1, sum/i
        x0 = x1
    end do
    close(33)
end program logistic_map_liapunov_2