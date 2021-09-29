program random_walker
    implicit none
    integer,parameter :: Nwalk  = 10
    integer,parameter :: Nstep = 100
    integer :: iwalk, istep, ir
    real(8) :: x,y
    real(8) :: drandom
    integer :: seed
    common /randoms/ seed

    seed = 374676287
    open(unit=20, file='dataR.dat')
    do iwalk = 1, Nwalk
        x = 0.0D0; y = 0.0D0
        open(unit=21, file='data.dat')
        do istep = 1,Nstep
            ir = INT(drandom()*4)
            select case(ir)
                case(0)
                x = x + 1.0D0
                case(1)
                x = x - 1.0D0
                case(2)
                y = y + 1.0D0
                case(3)
                y = y - 1.0D0
            end select
        end do
        close(21)
        call sleep(2)
        write(20,*) x*x+y*y
        call flush(20)
    end do
end program random_walker