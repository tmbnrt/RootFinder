!..... Numerical calculation of the derivatives of f(x) .....
!...............    f    =  f(x)          ...................
!...............    x    =  x-value       ...................
!...............    h    =  step width    ...................

real(8) function fs(f,x,h)
     implicit none

     real(8),external::f
     real(8)::x
     real(8)::h

     fs = (1.d0/h)*(f(x+h/2.d0) - f(x-h/2.d0))

end function fs
