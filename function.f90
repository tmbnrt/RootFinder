!..... INPUT FUNCTION .....
!....... Project TB .......
!........ functions .......


real(8) function f(x)
    implicit none

    real(8)::x


!... Function 1
     f = ( x*cos(x/2.d0) ) / ( 3.d0-x-4.d0*sin(x) )


!... Function 2
!     f = 5.d0*(x**6.d0) - 7.d0*(x**5.d0) + 2.d0*(x**4.d0) + (x**2.d0) &
!                        - x*10.d0 - 2.d0


!... Function 3
!       f = 2.d0*exp(-x/2.d0)*( 1.d0-x*sin(x/2.d0) ) &
!                          - 3.d0*exp(-x/3.d0)*( 1.d0-x*cos(x/3.d0) )


end
