real function newton(f,fs,fx,fsx,x,h,lb,ub,prec,pf,s,maxit)
    implicit none

    real(8),external::f                 ! Function
    real(8),external::fs                ! Derivative of the function

    real(8)::lb                         ! Lower bound
    real(8)::ub                         ! Upper bound
    real(8)::prec                       ! Precision
    real(8)::h                          ! Step width for slope calculation
    real(8)::x
    real(8)::fx
    real(8)::fsx
    real(8)::pf                         ! Precision variable (multiplier)
    real(8)::s                          ! Step width for bad slopes

    integer::maxit                      ! Max.# of iterations

!c...  Helpers
    integer::i                          ! Serial# for iteration steps
!----------------------------------------------------------------------
!c
    do i = 1,maxit                      ! loop over the max. possible iteration number
!c
!c... Next Newton cycle
      fx  = f(x)                        ! New function value
      fsx = fs(f,x,h)                   ! Function's derivative
!c
!c... Check the x value:  x inside the chosen interval?
      if (x < lb .or. x > ub) then
         exit                           ! exit newton if x is outside the interval
      endif
!c
!c... Check precision of the function
      if (dabs(fx) < prec) then
         newton = x
         exit
      else
!c
!c... Check the slope
         if (dabs(fsx) < prec*pf) then        ! for a bad slope
            write(*,*) "*** Bad slope --> use slope increment ***"
            x = x + s
!c
!c... For a good slope:
         else
            x = x - fx/fsx
         endif
      endif

    enddo                               ! loop over the max. possible iteration number

end function
