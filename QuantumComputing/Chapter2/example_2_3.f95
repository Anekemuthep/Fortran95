program example_2_3
    implicit none
    real*8 :: v(0:3)
    integer :: i
    v = (/1.0, 2.0, 3.0, 4.0/)
    call NORMALIZERVR(v,4)
    do i=0,3,1
        print*,v(i)
    end do
end program example_2_3

subroutine NORMALIZERVR(v,n)
    implicit none
    integer :: n,i
    real*8 :: v(0:n-1), c
    c = 0.0d0
    do i=0,n-1
        c = c + v(i)*v(i)
    end do
    v = v/dsqrt(c)
end subroutine NORMALIZERVR