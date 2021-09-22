!========================================
! Bifurcation Points
!========================================

program bifurcationPoints
    implicit none
    real(8), parameter :: tol = 1.0D0-10
    integer :: k, iter
    real(8) :: r0, x0
    real(8) :: A(2,2), B(2), dX(2)
    real(8) :: error
    real(8) :: F, dFdx, dFdr, d2Fdx2, d2Fdrdx
!----- Input
    print *, '# Enter k, r0, x0: '
    read *, k, r0, x0
    print *, '# Period k= ', k
    print *, '# r0= ', r0, ' x0=', x0
!---- Initialize
    error = 1.0D0
    iter  = 0
    do while (error .gt. tol)
        A(1,1) = 1.0D0-dFdx(k,x0,r0)
        A(1,2) = -dFdr     (k,x0,r0)
        A(2,1) = d2Fdx2    (k,x0,r0)
        A(2,2) = d2Fdrdx   (k,x0,r0)
        B(1)   = -x0 +    F(k,x0,r0)
        B(2)   = -dFdx     (k,x0,r0)-1.0D0
!---- Solve a 2x2 linear system:
        call solve2x2(A,B, dX)
        x0 = x0 + dX(1)
        r0 = r0 + dX(2)
        error = 0.5D0*sqrt(dX(1)**2+dX(2)**2)
        iter = iter + 1
        print*, iter, 'x0= ', x0, ' r0=', r0, ' err=', error
    end do
end program bifurcationPoints

!================================
! function F and its derivates
real(8) function F(k,x,r)
    implicit none
    real(8) :: x, r, x0
    integer k, i

    x0 = x
    do i=1,k
        x0 = r * x0 * (1.0D0-x0)
    end do
    F = x0

end function F
!----------------------------------
real(8) function dFdx(k,x,r)
    implicit none
    real(8) :: x, r, eps
    real(8) :: F
    integer k

    eps = 1.0D0-6*x
    dFdx = (F(k,x+eps,r)-F(k,x-eps,r))/(2.0D0*eps)
end function dFdx
!-----------------------------------
real(8) function dFdr(k,x,r)
    implicit none
    real(8) :: x, r, eps
    real(8) :: F
    integer k

    eps = 1.0D0-6*x
    dFdr = (F(k, x, r+eps)-F(k,x, r-eps))/(2.0D0*eps)
end function dFdr
!-------------------------------------
real(8) function d2Fdx2(k,x,r)
    implicit none
    real(8) :: x, r, eps
    real(8) :: F
    integer k

    eps = 1.0D0-6*eps
    d2Fdx2 = (F(k, x+eps,r)-2.0D0*F(k,x,r)+F(k,x-eps,r))/(eps*eps)
end function d2Fdx2
!------------------------------------
real(8) function d2Fdrdx(k,x,r)
    implicit none
    real(8) :: x, r, epsx, epsr
    real(8) :: F
    integer k

    epsx = 1.0D-6*x
    epsr = 1.0D-6*x
    d2Fdrdx = (F(k, x+epsx, r+epsr)-F(k, x+epsx,r-epsr) &
               -F(k, x-epsx, r+epsr)-F(k, x-epsx,r-epsr)) &
              /(4.0D0*epsx*epsr)
end function d2Fdrdx
!=============================================
subroutine solve2x2(A,b,dx)
    implicit none
    real(8) :: A(2,2), b(2), dx(2)
    real(8) :: num1, num2, det
    num1 = A(2,2)*b(1)-A(1,2)*b(2)
    num2 = A(1,1)*b(2)-A(2,1)*b(1)
    det = A(1,1)*A(2,2)-A(1,2)*A(2,1)
    if (det .eq. 0.0D0) stop 'solve 2x2: det=0'
    dx(1) = num1/det
    dx(2) = num2/det
end subroutine solve2x2