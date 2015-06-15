!  INCIDENT_FREE_DB.f90 
!
!  FUNCTIONS:
!  INCIDENT_FREE_DB - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: INCIDENT_FREE_DB
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************
module file_variables
    integer ::fileid_inc=11
    character(len=30) ::filename_inc="incident_period.txt"
    integer ::fileid_output=13
    character(len=30) ::filename_output="./log/incident_free_db.txt"

    end module
module variables
    integer i,j,k,ii
    integer incident_num
    integer,pointer,dimension(:) ::month1,day1,year1,hour1,minute1,month2,day2,year2,hour2,minute2,doy1,doy2,minute_of_year1,minute_of_year2,overlap
    integer status_1
    integer ::day_of_month(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)
    integer ref_minute1,ref_minute2
    integer check_day,test_minute
    end module
    program INCIDENT_FREE_DB
    use file_variables
    use variables
    implicit none
    open (fileid_inc,file=filename_inc)
        incident_num=0
        do while (.true.)
            read(fileid_inc,*,iostat=status_1)
            if (status_1.lt.0) exit
            incident_num=incident_num+1
        enddo
    close(fileid_inc)
    call ALL_ALLOCATE
    open (fileid_inc,file=filename_inc)
        do i = 1, incident_num
            read(fileid_inc,*) month1(i),day1(i),year1(i),hour1(i),minute1(i),month2(i),day2(i),year2(i),hour2(i),minute2(i),overlap(i)
            write(*,*) month1(i),day1(i),hour1(i),minute1(i)
            doy1=0
            doy2=0
            if (month1(i).ne.1) then
                do j = 1, month1(i)-1
                    doy1(i)=doy1(i)+day_of_month(j)
                enddo
            endif
                doy1(i)=doy1(i)+day1(i)
            if (month2(i).ne.1) then
                do j = 1, month2(i)-1
                    doy2(i)=doy2(i)+day_of_month(j)
                enddo
            endif
                doy2(i)=doy2(i)+day2(i)
            write(*,*) doy1(i),doy2(i)
            minute_of_year1(i)=1440*(doy1(i)-1)+60*hour1(i)+minute1(i)
            minute_of_year2(i)=1440*(doy2(i)-1)+60*hour2(i)+minute2(i)
            write(*,*) minute_of_year1(i),minute_of_year2(i)
        enddo
    close(fileid_inc)
    read(*,*)
    ref_minute1=288*68+24
    ref_minute2=288*68+36
    open(fileid_output,file=filename_output)
    do i = 0,6
        do k = 0,1439
            do j = 1, 365
                check_day=0
                if (mod(j+1,7) .eq.i) then
                
                    test_minute=(j-1)*288*5+k
                    do ii= 1, incident_num
                        if ((test_minute.ge.minute_of_year1(ii)).and.(test_minute.le.minute_of_year2(ii)+20)) then
                        check_day=1
                        exit
                        endif
                    enddo
                    !if (check_day.eq.1) 
                
                write(fileid_output,*) i, k, j, check_day             
            endif
            enddo                
        end do
    end do
    close(fileid_output)
    deallocate(month1,month2,day1,day2,year1,year2,hour1,hour2,minute1,minute2,doy1,doy2,minute_of_year1,minute_of_year2,overlap)
    end program INCIDENT_FREE_DB

subroutine ALL_ALLOCATE
use variables
implicit none
allocate(month1(incident_num))
allocate(day1(incident_num))
allocate(year1(incident_num))
allocate(hour1(incident_num))
allocate(minute1(incident_num))
allocate(month2(incident_num))
allocate(day2(incident_num))
allocate(year2(incident_num))
allocate(hour2(incident_num))
allocate(minute2(incident_num))
allocate(doy1(incident_num))
allocate(doy2(incident_num))
allocate(minute_of_year1(incident_num))
allocate(minute_of_year2(incident_num))
allocate (overlap(incident_num))
    end subroutine