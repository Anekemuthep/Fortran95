program example_2_1
    implicit none
    real(8) :: v1(0:3), v2(0:3), c
    v1 = (/1.0,2.0,3.0,4.0/)
    v2 = (/2.0,3.0,1.0,-1.0/)
    call IPVR(v1,v2, 4,c)
    print*, c
end program example_2_1

subroutine IPVR(v1, v2, n, c)
    implicit none
    integer :: n, i
    real(8) :: v1(0:n-1), v2(0:n-1),c
    c = 0.0d0
    do i=0,n-1
        c=c+v1(i)*v2(i)
    end do
end subroutine IPVR