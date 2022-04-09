program example_2_5
    implicit none
    real*8 :: A(0:1,0:1),B(0:1,0:1),C(0:1,0:1)
    integer :: i
    A(0,:) = (/1.0, 2.0/)
    A(1,:) = (/2.0, -1.0/)
    B(0,:) = (/1.0, 2.0/)
    B(1,:) = (/3.0,1.0/)
    call MMULMR(A,B,2,2,2,C)
    do i=0,1,1
        write(111,*) C(i,:)
    end do
end program example_2_5

subroutine MMULMR(A,B,m,n,p,C)
    implicit none
    integer :: n,m,l,i,j,k,p
    real*8 :: A(0:m-1, 0:n-1), B(0:n-1,0:p-1), C(0:m-1, 0:p-1), temp
    do i=0,m-1,1
        do j=0,p-1,1
            temp = 0.0d0
            do k=0,n-1,1
                temp =temp + A(i,k)*B(k,j)
            end do
            C(i,j) = temp
        end do
    end do
end subroutine MMULMR