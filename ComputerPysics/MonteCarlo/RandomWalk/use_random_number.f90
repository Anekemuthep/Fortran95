program use_random_number
    implicit none
    integer :: NSEEDS
    integer, allocatable :: seeds(:)
    integer :: seed
    real(8) :: r
    integer, parameter :: NR=20
    real(8), dimension(NR) :: randoms
    integer(8) :: icount
    integer :: i

    seed = 47279823

    call RANDOM_SEED(size=NSEEDS)
    ALLOCATE(seeds(NSEEDS))

    seeds = seed + 37 * (/ (i-1, i=1,NSEEDS)/)

    call RANDOM_SEED(PUT=seeds)

    do icount = 1,10
        call random_number(r)
        print*, r
    end do

    call random_number(randoms)
    print '(1000G28.17)', randoms

    open(unit=11, file='rannum.seed')
    call RANDOM_SEED(GET=seeds)
    write(11, '(5I20)') seeds
    close(11)

    call random_number(randoms)
    print '(A,1000G28.17)', '#FIRST : ', randoms

    open(unit=11, file='rannum.seed')
    read(11,*) seeds
    call RANDOM_SEED(PUT=seeds)

    call random_number(randoms)
    print '(A,1000G28.17)', '#SECOND: ', randoms
end program use_random_number