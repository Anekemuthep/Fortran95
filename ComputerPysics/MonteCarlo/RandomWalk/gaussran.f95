program gaussianran
    implicit none
    real(8), parameter :: sigma = 1.0D0
    real(8) :: r, phi
    logical, save :: new = .TRUE.
    real(8), save :: x
    real(8), parameter :: PI2 = 6.28318530717958648D0
    real(8) :: drandom
    if(new)then
        new = .FALSE.
        r = drandom()
        phi = PI2*drandom()
        r = sigma*sqrt(-2.0D0*log(r))
        x = r*cos(phi)
        gaussran = r*sin(phi)
    else
        new = .TRUE.
        gaussran = x
    end if
end program gaussianran