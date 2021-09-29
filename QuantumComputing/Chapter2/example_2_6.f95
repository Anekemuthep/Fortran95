program example_2_6
    implicit none
    real*8 :: A(0:1,0:0), B(0:1,0:1), C(0:3, 0:1)
    integer :: i
    A(0,0) = 1.0
    A(1,0) = 2.0
    B(0,:) = (/1.0, 2.0/)
    B(1,:) = (/3.0,1.0/)
    call KPMR(2,1,A,2,2,B,C)
    do i=0,3,1
        write(111,*) C(i,:)
    end do
end program example_2_6

subroutine KPMR(m,n,A,p,q,B,C)
    implicit none
    integer :: m,n,p,q,i,j,k,l
    real*8, dimension(0:m-1, 0:n-1) :: A
    real*8, dimension(0:p-1,0:q-1) :: B
    real*8, dimension(0:m*p-1, 0:n*q-1) :: C
    do k=0,m-1,1
        do l=0,p-1,1
            do i=0,n-1,1
                do j=0,q-1,1
                    C(p*k+l,q*i+j) = A(k,i) * B(l,j)
                end do
            end do
        end do
    end do
end subroutine KPMR