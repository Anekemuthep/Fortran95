program correlations2
    implicit none
    integer, parameter :: L = 10000
    integer :: i,N
    character(10) :: arg
    real(8) :: naiveran, drandom
    integer :: seed
    common /randoms/ seed

    if(IARGC() .EQ. 1) then
        call GETARG(1, arg); read(arg,*)N
    else
        N=1000
    end if
    seed = 348325
    do i=1,N
        print*, INT(L * naiveran()), INT(L * naiveran())
    end do
end program correlations2