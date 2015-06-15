
module file_variables
    integer ::fileid_inc=11
    integer ::fileid_up=13
    integer ::fileid_down=15
    character(len=30) ::filename_inc="./log/output.txt"
    integer ::fileid_output=9
    character(len=30) ::filename_output="./log/incident_impact.txt"
    end module
module variables
    integer i,j,k
    integer incident_num    ! total number of incidents
    integer last_incident   ! for new incident detection
    integer last_down_station,last_up_station
    integer incident_count  ! count the number of compared days for each incident
    integer t_incident_id,t_incident_day,t_com_day,t_hour,t_minute,t_up_sta,t_down_sta
    real    t_station_coef,t_del_60    ! for incident input file
    real    station_coef    ! station_coef for each incident
    integer status_1        ! status for reading incident input file
    real    sum_delay       ! total delay for each incident
    real    :: inc_delay(60)! delay for each compared day 
    integer :: inc_doy(60),inc_hour(60),inc_minute(60)
    real    chi             ! parameter for distribution estimation
    real    :: chi_square(81)! chi-square table
    real    max_delay       ! delay range from the parameter estimation 95%
    character(len=30) ::up_station_str,down_station_str ! up station, down station ids
    real time_coef          ! time coefficient for the neighbour time period
    integer line_num        ! where to start reading available data
    integer ref_line        ! the reference line
    integer temp1,temp2,temp3 ! dummy for reading
    real del_up_bef,del_up_aft,del_down_bef,del_down_aft ! upstream, downstream, before incident, after incident delay
    real end_inc_del        ! delay when the incident ended
    integer period_count    ! count the number of period till when the delay vanished
    real    before_period,after_period !the delay before & after each selected segment
    integer before_doy,before_hour,before_minute,after_doy,after_hour,after_minute ! time for before and after period time points
    integer up_doy,up_hour,up_minute,down_doy,down_hour,down_minute! time for upstream and downstream station
    real    up_del,down_del! delay for up and down stream while looking for the last period
    real    temp_time_coef ! temporary time coefficient for interpolation in each time period
    real    temp_delay     ! temporary delay for test interpolation 
    integer ::station_id(100) ! store the station id information
    real    ::station_mp(100) ! store the station milepost information
    integer station_number    !the total number of stations
    integer test_int1,test_int2,test_int3
    integer test_real1
    end module
    program distribution_estimator
    use file_variables
    use variables
    implicit none
    incident_num=1
    last_incident=0
    incident_count=0
    open (4,file="STA_ID.txt")
    read (4,*) station_number
    do i = 1, station_number
        read(4,*) station_mp(i),station_id(i)
    enddo
    
    close (4)
    open (4,file="Chi_Square.txt")
    do i = 1, 81
        read (4,*) chi_square(i)
    enddo
    close(4)
    open (fileid_inc,file=filename_inc)
    open (fileid_output,file=filename_output)
    do while(.true.)
        read (fileid_inc,*,iostat=status_1) t_incident_id,t_incident_day,t_com_day,t_hour,t_minute,t_up_sta,t_down_sta,t_station_coef,t_del_60
        !if ((status_1.lt.0)) exit
        if (((t_incident_id.ne.last_incident).and.(last_incident.ne.0)).or.(status_1.lt.0)) then
            sum_delay=0
            do i = 1, incident_count
                sum_delay=sum_delay+inc_delay(i)
            enddo
            
            if (sum_delay.gt.1e-4) then
                !calculate max delay
                
                    chi=chi_square(incident_count*2-29)
                
                
                max_delay=2*sum_delay/chi
            else
                !output delay is 0
                max_delay=0
            endif
            
                !max_delay is available, locate incident end time
            write(up_station_str,"(I6)") station_id(last_up_station)
            write(down_station_str,"(I6)") station_id(last_down_station)
            write(*,*) LAST_INCIDENT,"UP STATION IS", trim(UP_STATION_STR)," DOWN STATION IS ",trim(DOWN_STATION_STR)
            if (t_up_sta.ne.t_down_sta) then
                open (fileid_up,file="./output/N_"//trim(adjustl(up_station_str))//"_DEL60.txt")
                open (fileid_down,file="./output/N_"//trim(adjustl(down_station_str))//"_DEL60.txt")
                
                time_coef=(inc_minute(incident_count)-inc_minute(incident_count)/5*5)/5.0
                line_num=(inc_doy(incident_count)-1)*288+12*inc_hour(incident_count)+inc_minute(incident_count)/5+1
                
                ref_line=68*288+12*2
                if (line_num.gt.ref_line) then
                    line_num=line_num-12
                endif
                do i = 1, line_num-1
                    read (fileid_up,*) test_int1,test_int2,test_int3,test_real1
                enddo
                do i = 1, line_num-1
                    read (fileid_down,*)
                enddo
                
                
                read (fileid_up,*) temp1,temp2,temp3,del_up_bef
                read (fileid_down,*) temp1,temp2,temp3,del_down_bef
                read (fileid_up,*) temp1,temp2,temp3,del_up_aft
                read (fileid_down,*) temp1,temp2,temp3,del_down_aft
             
                end_inc_del=(del_up_bef*(1-time_coef)+time_coef*del_up_aft)*(1-station_coef)+station_coef*(del_down_bef*(1-time_coef)+time_coef*del_down_aft)
                
                if (end_inc_del.le.max_delay) then
                    write(fileid_output,*) "1",last_incident, 5*288*(inc_doy(incident_count)-1)+5*12*inc_hour(incident_count)+inc_minute(incident_count),last_up_station,last_down_station
                    close(fileid_up)
                    close(fileid_down)
                else
                    period_count=0
                    before_period=end_inc_del
                    after_period=(1-station_coef)*del_up_aft+station_coef*del_down_aft
                    before_doy=inc_doy(incident_count)
                    before_hour=inc_hour(incident_count)
                    before_minute=inc_minute(incident_count)
                    after_doy=temp1
                    after_hour=temp2
                    after_minute=temp3
                    write(*,*) "START TO LOCATE NEW PERIOD"
                    do while ((after_period.gt.max_delay).and.(period_count.lt.5).and.((288*(up_doy-1)+12*up_hour+up_minute/5).lt.105107))
                        
                        period_count=period_count+1
                        read(fileid_up,*) up_doy,up_hour,up_minute,up_del
                        read(fileid_down,*) down_doy,down_hour,down_minute,down_del
                        
                        before_period=after_period
                        before_doy=after_doy
                        before_hour=after_hour
                        before_minute=after_minute
                        after_doy=down_doy
                        after_hour=down_hour
                        after_minute=down_minute
                        after_period=(1-station_coef)*up_del+station_coef*down_del
                        
                    enddo
                    if ((period_count.ge.5).or.((288*(up_doy-1)+12*up_hour+up_minute/5).ge.105107)) then
                        write(fileid_output,*) "2",last_incident,(after_doy-1)*288*5+5*12*after_hour+after_minute,last_up_station,last_down_station
                        close(fileid_up)
                        close(fileid_down)
                    else
                    do i = ((before_doy-1)*288*5+5*12*before_hour+before_minute),((after_doy-1)*288*5+5*12*after_hour+after_minute)
                        
                        temp_time_coef=1.0*(i-before_doy*288*5-5*12*before_hour-before_minute)/(after_doy*288*5+5*12*after_hour+after_minute-before_doy*5*288-5*12*before_hour-before_minute)
                        temp_delay=(1-temp_time_coef)*before_period+temp_time_coef*after_period
                        
                        if (temp_delay.le.max_delay) then
                            write(fileid_output,*) "3",last_incident,i,last_up_station,last_down_station
                            exit
                        endif
                    enddo
                    endif
                    close(fileid_up)
                    close(fileid_down)
                    
                endif
                
            else
                open (fileid_up,file="./output/N_"//trim(adjustl(up_station_str))//"_DEL60.txt")
                time_coef=(inc_minute(incident_count)-inc_minute(incident_count)/5*5)/5.0
                line_num=(inc_doy(incident_count)-1)*288+12*inc_hour(incident_count)+inc_minute(incident_count)/5
                ref_line=68*288+12*2
                if (line_num.gt.ref_line) then
                    line_num=line_num-12
                endif
                do i = 1, line_num-1
                    read (fileid_up,*) 
                enddo
                read (fileid_up,*) temp1,temp2,temp3,del_up_bef
                read (fileid_up,*) after_doy,after_hour,after_minute,del_up_aft
                end_inc_del=(1-time_coef)*del_up_bef+time_coef*del_up_aft
                if (end_inc_del.le.max_delay) then
                    write(fileid_output,*) "1",last_incident, 5*288*(inc_doy(incident_count)-1)+12*5*inc_hour(incident_count)+inc_minute(incident_count),last_up_station,last_down_station
                    close(fileid_up)
                else
                       
                    period_count=0
                    before_period=end_inc_del
                    after_period=del_up_aft
                    before_doy=inc_doy(incident_count)
                    before_hour=inc_hour(incident_count)
                    before_minute=inc_minute(incident_count)
                    write(*,*) "START TO LOCATE NEW TIME PERIOD"
                    do while ((after_period.gt.max_delay).and.(period_count.lt.5).and.((288*(up_doy-1)+12*up_hour+up_minute/5).lt.105107))
                        period_count=period_count+1
                        read(fileid_up,*) up_doy,up_hour,up_minute,up_del
                        before_period=after_period
                        
                        after_period=(1-station_coef)*up_del+station_coef*down_del
                            
                        before_doy=after_doy
                        before_hour=after_hour
                        before_minute=after_minute
                        after_doy=up_doy
                        after_hour=up_hour
                        after_minute=up_minute
                        
                    enddo
                    if ((period_count.ge.5).or.((288*(up_doy-1)+12*up_hour+up_minute/5).ge.105107)) then
                        write(fileid_output,*) "2",last_incident,(after_doy-1)*288*5+5*12*after_hour+after_minute,last_up_station,last_down_station
                        close(fileid_up)
                    else
                    do i = ((before_doy-1)*288+12*before_hour+before_minute),((after_doy-1)*288+12*after_hour+after_minute)
                        temp_time_coef=1.0*(i-before_doy*288-12*before_hour-before_minute)/(after_doy*288+12*after_hour+after_minute-before_doy*288-12*before_hour-before_minute)
                        temp_delay=(1-temp_time_coef)*before_period+temp_time_coef*after_period
                        if (temp_delay.le.max_delay) then
                            write(fileid_output,*) "3",last_incident,i,last_up_station,last_down_station
                            exit
                        endif
                    enddo
                    close(fileid_up)
                    endif
                endif
                
            endif
            if (status_1.lt.0) exit
            last_incident=t_incident_id
            last_up_station=t_up_sta
            last_down_station=t_down_sta
            incident_count=1
            inc_doy(incident_count)=t_incident_day
            inc_delay(incident_count)=t_del_60
            station_coef=t_station_coef
        else
           incident_count=incident_count+1
           inc_delay(incident_count)=t_del_60
           inc_hour(incident_count)=t_hour
           inc_minute(incident_count)=t_minute
           inc_doy(incident_count)=t_incident_day
           station_coef=t_station_coef
           last_up_station=t_up_sta
           last_down_station=t_down_sta
           last_incident=t_incident_id
        endif
    enddo
    close(fileid_output)
    close(fileid_inc)
    end program distribution_estimator

