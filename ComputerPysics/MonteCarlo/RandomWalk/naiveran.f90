real(8) function naiveran()
    implicit none
    integer :: iran = 13337
    common /naiveranpar/ iran
    integer, parameter :: m = 131072
    integer, parameter :: a = 1277

    iran = a*iran
    iran = MOD(iran,m)

    naiveran = iran/DBLE(m)
end function naiveran