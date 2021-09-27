program example_2_4
    implicit none
    real*8 :: v1(0:1), v2(0:2), A(0:1,0:2)
    integer :: i
    v1 = (/1.0, 2.0/)
    v2 = (/2.0, 3.0, 1.0/)
    call OPVR(v1,v2,2,3,A)
    do i = 0,1,1
        write(111,*)A(i,:)
    end do
end program example_2_4

subroutine OPVR(v,w,n1,n2,A)
    implicit none
    integer :: n1, n2, i, j
    real*8 :: v(0:n1-1), w(0:n2-1), A(0:n1-1,0:n2-1)
    do i=0,n1-1
        do j=0,n2-1
            A(i,j)=v(i)*w(j)
        end do
    end do
end subroutine OPVR