program example_2_8
    implicit none
    real*8 :: A(0:1,0:1), B(0:1,0:1), C(0:1,0:1)
    integer :: i
    A(0,:) = (/1.0,2.0/)
    A(1,:) = (/2.0,-1.0/)
    B(0,:) = (/1.0,2.0/)
    B(1,:) = (/3.0,1.0/)
    call COMMR(2,A,B,C)
    do i=0,1,1
        write(111,*) C(i,:)
    end do
end program example_2_8

subroutine COMMR(n,A,B,C)
    implicit none
    integer :: n
    real*8 :: A(0:n-1, 0:n-1), B(0:n-1, 0:n-1), C(0:n-1, 0:n-1)
    C = matmul(A,B) - matmul(B,A)
end subroutine COMMR