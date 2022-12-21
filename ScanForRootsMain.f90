program ScanForRootsMain
    implicit none

    real(8),external::f             !declaration of the function
    real(8),external::fs            !the derivative
    integer,external::ScanForRoots  !newton algorithm
    integer,external::readData      !read the input data

    integer::roots                  !maximum number of roots to find
    real(8)::lb                     !lower bound
    real(8)::ub                     !upper bound
    real(8)::sw                     !step calculating the roots
    real(8)::prec                   !precision
    integer::maxit                  !max iterations
    real(8)::h                      !step for slope calculation
    character(256)::inpfile         !name of the input file

    integer::nret                   !return code of the newton
    integer::rcount                 !number of found roots

    integer::iarg,i                 !number of command options
    character(256)::arg             !option string

    ! set name of input file
    inpfile = 'function1.inp'

    !analyse the command line options
    iarg = iargc()                  !get the option number
    write(*,'(i2,a)') iarg," option(s)"
    do i = 0, iarg
        call getarg(i,arg)
        write(*,'(a,i2,a,a)') "option ",i,": ",arg
    end do

    !use the command line options
    if (iarg > 0) then              !if (iarg > 0) then  call getarc(1,inpfile)
        call getarg(1,inpfile)
    endif


    !read the input data
    if (readData(inpfile,roots,h,lb,ub,sw,prec,maxit) < 0) then
        write(*,*) "***ERROR: Input data not found!***"
        stop
    endif

    !run the newton
    nret = ScanForRoots(f,lb,ub,sw,prec,h,maxit,roots,rcount)


end program

