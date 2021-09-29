program example_2_7
    implicit none
    real*8 :: A(0:2,0:2), trace
    A(0,:) = (/1.0,2.0,3.0/)
    A(1,:) = (/3.0,-2.0,-5.0/)
    A(2,:) = (/7.0,-2.0,-3.0/)
    call TRMR(A,3,trace)
    write(111,*) trace
end program example_2_7

subroutine TRMR(A,n, trace)
    integer :: i,n
    real*8 :: A(0:n-1,0:n-1), trace
    trace = 0.0d0
    do i=0,n-1
        trace = trace + A(i,i)
    end do
end subroutine TRMR