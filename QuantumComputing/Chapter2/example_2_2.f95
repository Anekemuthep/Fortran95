program example_2_2
    implicit none
    real*8 :: v(0:3), norm
    v = (/1.0,2.0,3.0,4.0/)
    call NORMVR(v,4, norm)
    print*, norm
end program example_2_2

subroutine NORMVR(v,n,norm)
    implicit none
    integer :: n, i
    real*8 :: v(0:n-1), c, norm
    c = 0.0d0
    do i=0,n-1
        c = c+v(i)*v(i)
    end do
    norm=dsqrt(c)
end subroutine NORMVR