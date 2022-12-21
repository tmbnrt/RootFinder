integer function ScanForRoots(f,lb,ub,sw,prec,h,maxit,roots,rcount)
    implicit none

    real(8),external::f         ! Function
    real(8),external::fs        ! Derivative of the function
    real(8),external::newton    ! Newton algorithm

    real(8)::x0                 ! Starting point
    real(8)::lb                 ! Lower bound
    real(8)::ub                 ! Upper bound
    real(8)::sw                 ! Step width for root calculation
    real(8)::prec               ! Precision
    real(8)::h                  ! Step width for slope calculation
    real(8)::x
    real(8)::fx
    real(8)::fsx

    real(8)::pf = 0.0005        ! Precision variable (multiplier)
    real(8)::racc = 1.d-5       ! Root accuracy for multiple roots
    real(8)::roundr = 1.d-5     ! Rounding for root output

    real(8)::s  = 0.10          ! Step width for bad slopes

    integer::maxit              ! Max.# of iterations
    integer::roots              ! Max.# of roots

!c...  Helpers
    !integer::i                  ! Serial# for iteration steps
    integer::rcount             ! Root counter
    integer::listr              ! Helper to list the calculated roots
    integer::hr                 ! Helper to print roots
    integer::rmulti

    real(8)::SoluVec(roots)     ! Solution list for calculated roots
!c
!----------------------------------------------------------------------
    rcount = 0                  ! set root counter to zero
    x0     = lb                 ! Start at lower bound
!c
    mainloop: do                ! start the main loop
!c
      x = x0                    ! update x for the next step
!c
!c... Check the bound
      if (x0 > ub) then
         write (*,*) " "
         write (*,*) "*****************************************"
         write (*,*) "********* CALCULATION COMPLETED *********"
         write (*,*) "*****************************************"
         write (*,*) " "
         write (*,*) "Number of found roots =",rcount
!
         write(*,*) '     '
         write(*,*) '     '
         write(*,*) '       CALCULATED ROOTS       '
         write(*,*) '     '
         do listr = 1,rcount
            write (*,'(i3,f12.6)') listr,SoluVec(listr)
         end do
         write(*,*) '     '
         ScanForRoots = -1
         exit
      endif
!c
!c... Check: max number of roots found?
      if (rcount >= roots) then
         write(*,*) " *** Maximum number of roots found ***"
         ! Print number of found roots
         write(*,*) " Number of found roots: ",rcount
         ! Print the found roots
         do hr = 1,rcount
            write(*,*) " root ",hr," = ",SoluVec(hr)
         end do
         ScanForRoots = -2
         exit                           ! Maximum # of roots found --> EXIT
      endif
!c
!c... Newton cycle
      x = newton(f,fs,fx,fsx,x,h,lb,ub,prec,pf,s,maxit)
!c
!c... Check the x value:  x inside the chosen interval?
      if (x < lb .or. x > ub) then
         !write(*,*) "*** x outside the chosen interval ***"
         x0 = x0 + sw
         cycle mainloop
      endif
!c
!c... Check for already found roots
         do rmulti = 1,rcount
            if (dabs(x - SoluVec(rmulti)) < racc) then                      !!!! geändert:  < prec*racc !!!!!!
               !write(*,*) "*** Multiple root - already found:  ***",x
               x0 = x0 + sw
               cycle mainloop
            endif
         end do
!c
!c... New root found: increment root counter & write root into 'SoluVec'
         rcount = rcount + 1
         if (dabs(x) > roundr) then
             SoluVec(rcount) = x
             x0 = x0 + sw
             cycle mainloop
         else
             SoluVec(rcount) = 0.d0
             x0 = x0 + sw
             cycle mainloop
         endif
!c
    end do mainloop

end function
