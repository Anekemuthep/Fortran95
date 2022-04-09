!==================================
! Newton Raphson fo two variables
!==================================
program NewtonRaphson2
    implicit none
    real(8), parameter :: eps = 1D-6
    integer, parameter :: NMAX = 1000
    real(8) :: A(2,2), b(2), dx(2)
    real(8) :: x, y, err
    integer :: i
    print *, 'Enter x0, y0: '
    read *, x, y
    err = 1.0D0
    print *, 'iter                   x             y               error       '
    print *, '-----------------------------------------------------------------'
    print *, 0, x, y, err
    do i=1,NMAX
        b(1) = -(2.0D0*x*x-3.0D0*x*y+y-2.0D0) ! -g1(x,y)
        b(2) = -(3.0D0*x+ x*y + y -1.0D0)     ! -g2(x,y)
        A(1,1) = 4.0D0*x-3.0D0*y ; A(1,2) = 1.0D0-3.0D0*x
        A(2,1) = 3.0D0+y         ; A(2,2) = 1.0D0+x
        call solve2x2(A,B, dx)
        x = x + dx(1)
        y = y * dx(2)
        err = 0.5D0*SQRT(dx(1)**2+dx(2)**2)
        print *, i, x, y, err
        if(err .lt. eps) exit
    end do
end program NewtonRaphson2
!===========================================================
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