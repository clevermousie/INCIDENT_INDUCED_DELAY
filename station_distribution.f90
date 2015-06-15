!  station_distribution.f90 
!
!  FUNCTIONS:
!  station_distribution - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: station_distribution
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************
module file_variables
    integer ::fileid_inc=11
    integer ::fileid_sta=13
    integer ::fileid_output=15
    integer ::fileid_target=17
    character(len=30) ::filename_inc="./log/incident_free_db.txt"
    character(len=30) ::filename_sta="STA_ID.txt"
    character(len=30) ::filename_output="./log/stations_dis.txt"
    end module
module variables
    integer i,j,k
    integer station_num
    integer,pointer,dimension(:) ::station_id
    real,pointer,dimension(:)    ::station_mp
    integer prev_minute
    integer t_dow,t_mod,t_select_doy,t_check_day
    integer status_1
    integer ::target_dow(100),target_mod(100),target_doy(100),target_check(100)
    integer day_count,real_day_count
    real    sum_delay,target_delay,delay_601,delay_602,sum_delay_sq
    integer line_num1,ref_line,line_num0
    integer temp1,temp2,temp3
    character(len=30) ::station_id_str
    real    time_coef
    integer start
    real    ::r2_check(70)
    end module
module fun_variables
    character(len=15) ::fun_output(55)
    integer fun_count
    end module
    program station_distribution
    use file_variables
    use variables
    use fun_variables
    implicit none
    call fun_subroutine
    fun_count=0
    open (fileid_sta,file=filename_sta)
    read (fileid_sta,*) station_num
    
    allocate(station_mp(station_num))
    allocate (station_id(station_num))
    
    do i = 1 ,station_num
        read (fileid_sta,*) station_mp(i),station_id(i)
    enddo
    close (fileid_sta)
    
    open (fileid_inc,file=filename_inc)
    open (fileid_output,file=filename_output)
    day_count=0
    prev_minute=0
    start=1
    do while (.true.)
        read (fileid_inc,*,iostat=status_1) t_dow,t_mod, t_select_doy, t_check_day
        
        if (((t_mod.eq.prev_minute).or.(start.eq.1)).and.(status_1.ge.0)) then
            if (start.eq.1) then
                start=0
            endif
            day_count=day_count+1
            target_dow(day_count)=t_dow
            target_mod(day_count)=t_mod
            target_doy(day_count)=t_select_doy
            target_check(day_count)=t_check_day
        else
            
            do j = 1, station_num
                sum_delay=0
                sum_delay_sq=0
                write (station_id_str,"(I6)") station_id(j)
                fun_count=fun_count+1
                if(fun_count.gt.55) then
                    fun_count=fun_count-55
                endif
                write (*,*) "start",station_id(j),target_dow(1),target_mod(1),fun_output(fun_count)
                open (fileid_target,file="./output/N_"//trim(adjustl(station_id_str))//"_Del60.txt")
                line_num0=0
                real_day_count=0
                do i = 1, day_count
                    if (target_check(i).eq.0) then
                            real_day_count=real_day_count+1
                            line_num1=(target_doy(i)-1)*288+target_mod(i)/5+1
                            ref_line=68*288+25
                            if (line_num1.ge.ref_line) then
                                line_num1=line_num1-12
                            endif
                            !write(*,*) line_num0,line_num1
                            !read(*,*)
                            time_coef=(target_mod(i)-target_mod(i)/5*5)/5.0
                            !write(*,*) line_num1,line_num0,line_num1-line_num0,i,day_count
                            do k = 1, line_num1-line_num0-1
                                read(fileid_target,*)
                            enddo
                            
                            read (fileid_target,*) temp1,temp2,temp3,delay_601
                            !if (delay_601.gt.0) then
                             !   write (*,*) temp1,temp2,temp3,delay_601
                                !read (*,*)
                            !endif
                            if (line_num1.lt.105108) then
                                read (fileid_target,*) temp1,temp2,temp3,delay_602
                            else
                                delay_602=delay_601
                            endif
                            !if (delay_602.gt.0) then
                            !    write (*,*) temp1,temp2,temp3,delay_602
                                !read(*,*)
                            !endif
                            target_delay=delay_601*(1-time_coef)+time_coef*delay_602
                            r2_check(real_day_count)=target_delay
                            !write (*,*) time_coef,target_delay
                            sum_delay=sum_delay+target_delay
                            sum_delay_sq=sum_delay_sq+target_delay*target_delay
                            line_num0=line_num1+1
                    endif
                    
                enddo
                close(fileid_target)
                !r2=r2_test
                write(fileid_output,*) target_dow(i),target_mod(i),station_id(j),sum_delay,sum_delay_sq,real_day_count
            enddo
            prev_minute=t_mod
            day_count=day_count+1
            target_dow(day_count)=t_dow
            target_mod(day_count)=t_mod
            target_doy(day_count)=t_select_doy
            target_check(day_count)=t_check_day
            day_count=0
            if (status_1.lt.0) exit
        endif
    
    enddo
    deallocate(station_mp,station_id)
    close(fileid_output)
    end program station_distribution

subroutine fun_subroutine
use fun_variables
implicit none
    integer ii
    fun_output(1)="_______________"
    fun_output(2:6)="       |       "
    fun_output(7)="_______|_______"
    fun_output(8:10)="              "
    fun_output(11)= fun_output(1)
    fun_output(12:16)="|      |      |"
    fun_output(17)="|             |"
    fun_output(18:20)=fun_output(8)
    fun_output(21)=fun_output(1)
    fun_output(22:27)="|              "
    fun_output(28:30)="               "
    fun_output(31)=fun_output(21)
    fun_output(32:37)=fun_output(22)
    fun_output(38:40)=fun_output(28)
    fun_output(41)=fun_output(1)
    fun_output(42:46)=fun_output(17)
    fun_output(47)="|_____________|"
    fun_output(48:55)=" "
    
    
end subroutine
!real function r2_test
!use variables
!implicit none
!real max,min
!integer r2_i
!    max=-1
!    min=1000
!    do r2_i=1, real_day_count
!        if (r2_check(r2_i).lt.min) then
!            min=r2_check(r2_i)
!        endif
!        if (r2_check(r2_i).gt.max) then
!            max=r2_check(r2_i)
!        endif
!    enddo
!    ending=min
!    y=0
!    do r2_i= 1, 10
!        start=ending+(max-min)/10
!        ending=start+(max-min)/10
!        y(r1_i)=exp(-1*start*2*sum_delay/chi_square(real_day_count*2-29))-exp(-1*ending*2*sum_delay/chi_square(real_day_count*2-29))
!        count=0
!        do r2_j=1, real_day_count
!            if ((r2_check(r2_j).ge.start).and.(r2_check(r2_j).lt.ending) then
!                count=count+1        
!            endif
!        enddo
!        y_est(r2_i)=count/real_day_count
!    enddo