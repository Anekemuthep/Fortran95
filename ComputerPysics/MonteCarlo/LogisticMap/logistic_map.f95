!=======================================
! Logistic Map
!=======================================

program logistic_map
    implicit none
    integer :: NSTEPS , i
    real(8) :: r, x0, x1
!---------- input
    print *, '# enter NSTEPS, r, x0: '
    read *, NSTEPS, r, x0
    print *, '#  NSTEPS = ', NSTEPS
    print *, '# r       = ', r
    print *, '# x0      = ', x0
!---------- Initialize
    open(unit=33, file='log.dat')
    write(33,*) 0, x0
!---------- Calculate
    do i=1, NSTEPS
        X1 = R * X0 * (1.0D0 -x0)
        write(33,*)i, x1
        x0 = x1
    end do
    close(33)
end program logistic_map