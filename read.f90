logical function readData(filename,roots,h,lb,ub,sw,prec,maxit)
    implicit none
!c
    character(*)::filename      ! 90 version
!   character*(*)::name          ! 77 qstandard
    integer::roots              ! Max.# of roots
    real(8)::lb                 ! Lower bound
    real(8)::ub                 ! Upper bound
    real(8)::prec               ! Precision
    integer::maxit              ! Max.# of iterations
    real(8)::sw                 ! Step width for root calculation
    real(8)::h                  ! Step width for slope calculation
!c
    integer::io = 10            ! channel to read
    integer::ioerr              ! return information of the open
    character(256)::buffer      ! input buffer
    integer::line = 0           ! line number
    character(256)::key         ! key
    integer::errors = 0         ! error counter
!----------------------------------------------------------------------
!c
!c... open the input file
    !    |cannel number (5:standard input - keyboard
    !                    6:standard output - screen)
    !                                            err = 900  <-jump to 900 if error!
    open(io,file=filename,status='old',iostat=ioerr)
    if (ioerr /= 0) then
        write(*,'(a,a,a)') "*** Error: ",filename(1:len_trim(filename))," not found"
        readData = .False.
        return
    end if
    write(*,*) '         INPUT DATA           '
!c
     do while(.True.)
!c
!c... read next line into a buffer
        line = line +1
        ! 77 kind:   err=100, end=200
        read(io,'(a256)',iostat=ioerr) buffer
        if (ioerr /= 0) then            ! ioerr > 0: read error, data error
            !write (*,*) 'ioerr:',ioerr  ! ioerr < 0: end of file
            exit
        end if
!c
        write(*,'(x,a)') buffer(1:len_trim(buffer))
!c
!c... ignore emty lines
!c... > write(*,*) "len(buffer).....:",len(buffer)
!c... > write(*,*) "len_trim(buffer):",len_trim(buffer)
        if (len_trim(buffer) < 1)   cycle
!c
!c... ignore comment lines
        if (buffer(1:1) == "#")     cycle
!c
!c... get the key
        read(buffer,*) key
!c
!c... analyse the key and read value
        if (key == "Max_number_of_roots") then
            read(buffer,*,iostat=ioerr) key,roots
            if (ioerr /= 0) then
                errors = errors +1
                write(*,'(a,i3,a)') "*** Error in line ",line,": Data error for roots"
            end if
!c
            elseif (key == "Lower_bound") then
            read(buffer,*,iostat=ioerr) key,lb
            if (ioerr /= 0) then
                errors = errors +1
                write(*,'(a,i3,a)') "*** Error in line ",line,": Data error for lb"
            end if
!c
            elseif (key == "Upper_bound") then
            read(buffer,*,iostat=ioerr) key,ub
            if (ioerr /= 0) then
                errors = errors +1
                write(*,'(a,i3,a)') "*** Error in line ",line,": Data error for ub"
            end if
!c
            elseif (key == "Step_width") then
            read(buffer,*,iostat=ioerr) key,sw
            if (ioerr /= 0) then
                errors = errors +1
                write(*,'(a,i3,a)') "*** Error in line ",line,": Data error for sw"
            end if
!c
            elseif (key == "Precision") then
            read(buffer,*,iostat=ioerr) key,prec
            if (ioerr /= 0) then
                errors = errors +1
                write(*,'(a,i3,a)') "*** Error in line ",line,": Data error for prec"
            end if
!c
            elseif (key == "Slope_step_width") then
            read(buffer,*,iostat=ioerr) key,h
            if (ioerr /= 0) then
                errors = errors +1
                write(*,'(a,i3,a)') "*** Error in line ",line,": Data error for h"
            end if
!c
            elseif (key == "Max_number_of_iterations") then
            read(buffer,*,iostat=ioerr) key,maxit
            if (ioerr /= 0) then
                errors = errors +1
                write(*,'(a,i3,a)') "*** Error in line ",line,": Data error for maxit"
            end if
!c
            if (prec < 0. .or. prec > 0.0001) then
            write(*,*) "*** Error: Bad precision value! ***"
            errors = errors +1
            goto 10
            endif
!c
            if (h < 0. .or. dabs(h) > 0.1) then
            write(*,*) "*** Error: Bad slope step width! ***"
            errors = errors +1
            goto 10
            endif
!c
           if (maxit < 2) then
           write(*,*) "*** Error: Bad iteration number! ***"
           errors = errors +1
           goto 10
           endif
!c
         end if
!c
    end do
!c
!c... close the file
!c...   status='keep'   -> do not delete
!c...   status='delete' -> delete file
 10    close(io)
    write (*,*) " "
    write(*,*) "Reading input file: COMPLETED"
    if (errors > 0) then
        readData = .False.
    else
        readData = .True.
    endif
!c
end function
