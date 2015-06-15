module file_variables
    integer ::fileid_inc=11
    character(len=30) ::filename_inc="./log/incident_impact.txt"
    integer ::fileid_sta=13
    character(len=30) ::filename_sta="STA_ID.txt"
    integer ::fileid_occ=15
    integer ::fileid_del=17
    integer ::fileid_output=19
    integer ::fileid_info=18
    character(len=30) ::filename_info="incident_info.txt"
    INTEGER ::FILEID_PER=21
    CHARACTER(LEN=30) ::FILENAME_PER="incident_period.txt"
    end module
module variables
    integer i,j,k
    integer station_num
    integer,pointer,dimension(:) ::station_id
    real,pointer,dimension(:) ::station_mp
    integer incident_id,minute_of_year,up_station,down_station,incident_doy,incident_hour,incident_minute
    integer line_num,ref_line
    real    time_coef
    integer target_station
    integer status_1,status_2,STATUS_3
    character(len=20) ::target_station_str
    integer doy1,hour1,minute1,doy2,hour2,minute2
    real    occupancy1,occupancy2,target_occupancy
    real    del601,del602,target_del60
    integer incident_count
    character(len=30) ::dummy
    integer ::pri(2000),impact(2000),sever(2000)
    integer ::start_month(2000),start_day(2000),start_year(2000),start_hour(2000),start_minute(2000)
    integer ::end_month(2000),end_day(2000),end_year(2000),end_hour(2000),end_minute(2000)
    integer start_line,start_moy,start_doy
    integer ::day_of_month(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)
    real    total_delay
    end module
    
    program OCCUPANCY_DETECT
    use file_variables
    use variables
    implicit none
    
    ! READ IN STATION INFORMATION
    open (fileid_sta,file=filename_sta)
    
        read (fileid_sta,*) station_num
        allocate(station_mp(station_num))
        allocate(station_id(station_num))
        do i = 1 , station_num
            read (fileid_sta,*) station_mp(i),station_id(i)
        enddo
    close (fileid_sta)
    
    ! READ IN INCIDENT INFORMATION
    ! INCIDENT_INFO : SEVERITY, PRIORITY, IMPACT
    ! INCIDENT_PERIOD: START TIME AND END TIME
    open (fileid_info,file=filename_info)
    open (fileid_per,file=filename_per)
    incident_count=1
    do while (.true.)
        read(fileid_info,*,iostat=status_2) dummy,pri(incident_count),impact(incident_count)
        read(fileid_per,*,iostat=status_3) start_month(incident_count),start_day(incident_count),start_year(incident_count),start_hour(incident_count),start_minute(incident_count)&
        ,end_month(incident_count),end_day(incident_count),end_year(incident_count),end_hour(incident_count),end_minute(incident_count)
        
        if (status_2.ne.status_3) then
            write(*,*) "INCIDENT INFO & INCIDENT PERIOD ARE NOT MATCH"
            STOP
        endif
        if(status_2.lt.0) exit
        
        ! SEVERITY QUANTIFICATION: HIGH 3, MEDIUM 2, LOW 1
        
        if (dummy(1:1).eq."M") then
            sever(incident_count)=2
        else
            if (dummy(1:1).eq."L") then
                sever(incident_count)=1
            else
                if(dummy(1:1).eq."H") then
                    sever(incident_count)=3
                endif
            endif
        endif
        incident_count=incident_count+1
    enddo        
    
    incident_count=incident_count-1
    WRITE(*,*) INCIDENT_COUNT
    
    ! START TO PROCESS CRASH IMPACT FILE
    
    open (fileid_inc,file=filename_inc)
    open (fileid_output,file="./log/occupancy_station.txt")
    do while(.true.)
        ! INCIDENT, THE MINUTE WHEN INCIDENT'S IMPACT DISMISSED, UPSTREAM STATION, DOWNSTREAM STATION
        read(fileid_inc,*,iostat=status_1) incident_id,minute_of_year,up_station,down_station
        if(status_1.lt.0) exit
        incident_doy=minute_of_year/1440+1
        incident_hour=(minute_of_year-1440*(incident_doy-1))/60
        incident_minute=minute_of_year-1440*(incident_doy-1)-60*incident_hour
        
        start_doy=0
        if (start_month(incident_id).gt.1) then
            do i = 1, start_month(incident_id)-1
                start_doy=start_doy+day_of_month(i)
            enddo
        endif
        start_doy=start_doy+start_day(incident_id)
        start_moy=(start_doy-1)*1440+60*start_hour(incident_id)+start_minute(incident_id)
        
        start_line=288*(start_doy-1)+12*start_hour(incident_id)+start_minute(incident_id)/5+1
        
        line_num=288*(incident_doy-1)+12*incident_hour+incident_minute/5+1
        ref_line=288*68+12*2+1
        if(line_num.gt.ref_line)then
            line_num=line_num-12
        endif
        if(start_line.gt.ref_line) then
            start_line=start_line-12
        endif
        time_coef=(incident_minute-incident_minute/5*5)/5.0
        target_station=up_station
        do while (.true.)
            if (target_station.lt.1) then
                write(fileid_output,*) "ERROR FOR INCIDENT", incident_id
                close(fileid_occ)
                exit
            endif
            write(target_station_str,"(I6)") station_id(target_station)
            open (fileid_occ,file="./output/N_"//trim(adjustl(target_station_str))//"_Occupancy.txt")
                do i = 1, line_num-1
                    read(fileid_occ,*)
                enddo
                read (fileid_occ,*) doy1,hour1,minute1,occupancy1
                read (fileid_occ,*) doy2,hour2,minute2,occupancy2
                
                target_occupancy=(1-time_coef)*occupancy1+time_coef*occupancy2
                !write(*,*) trim(adjustl(target_station_str)),' ', target_occupancy
                if (target_occupancy.le.0.35) then
                    write(*,*) "incident",incident_id
                    write(*,*) "incident from",start_day(incident_id),start_hour(incident_id),start_minute(incident_id),"to",end_day(incident_id),end_hour(incident_id),end_minute(incident_id)
                    write(*,*) "impact dismissed at",incident_doy,incident_hour,incident_minute
                    
                    open( fileid_del,file="./output/N_"//trim(adjustl(target_station_str))//"_DEL60.txt")
                    if (start_line.eq.line_num) then
                        do j = 1, start_line-1
                            read(fileid_del,*)
                        enddo
                        read(fileid_del,*) doy1,hour1,minute1,del601
                        write(*,*) doy1,hour1,minute1,del601
                        read(fileid_del,*) doy2,hour2,minute2,del602
                        write(*,*) doy2,hour2,minute2,del602
                        target_del60=(1-time_coef)*del601+time_coef*del602
                        
                        total_delay=(minute_of_year-start_moy)*del601+(mod(minute_of_year,5)*mod(minute_of_year,5)-mod(start_moy,5)*mod(start_moy,5))*(del602-del601)/10.0
                        write(fileid_output,"(I6,I7,2(I10),(F12.5),3(I4))",advance='no') incident_id,TARGET_STATION,start_moy,minute_of_year,total_delay,sever(incident_id),pri(incident_id),impact(incident_id)
                        write(fileid_output,*)
                        close(fileid_del)
                        if (mod(incident_id,10).eq.0) then
                            write(*,*) incident_id," is done"
                        endif
                    else
                        if ((line_num-1).eq.start_line)then

                            do j = 1, start_line-1
                                read(fileid_del,*)
                            enddo
                            read(fileid_del,*) doy1,hour1,minute1,del601
                            write(*,*) doy1,hour1,minute1,del601
                            
                            read(fileid_del,*) doy2,hour2,minute2,del602
                            write(*,*) doy2,hour2,minute2,del602
                            
                            total_delay=(5-mod(start_moy,5))*del601+(25-mod(start_moy,5)*mod(start_moy,5))*(del602-del601)/10.0
                            read(fileid_del,*) doy1,hour1,minute1,del601
                            write(*,*) doy1,hour1,minute1,del601
                            total_delay=total_delay+mod(minute_of_year,5)*del602+(mod(minute_of_year,5)*mod(minute_of_year,5))*(del601-del602)/10.0
                            write(fileid_output,"(I6,I7,2(I10),(F12.5),3(I4))",advance='no') incident_id,TARGET_STATION,start_moy,minute_of_year,total_delay,sever(incident_id),pri(incident_id),impact(incident_id)
                        write(fileid_output,*)
                        close(fileid_del)
                        if (mod(incident_id,10).eq.0) then
                            write(*,*) incident_id," is done"
                        endif
                        else
                            
                            do j = 1, start_line-1
                                read(fileid_del,*)
                            enddo
                            
                            read(fileid_del,*) doy1,hour1,minute1,del601
                            write(*,*) doy1,hour1,minute1,del601
                            read(fileid_del,*) doy2,hour2,minute2,del602
                            write(*,*) doy2,hour2,minute2,del602
                            total_delay=(5-mod(start_moy,5))*del601+(25-mod(start_moy,5)*mod(start_moy,5))*(del602-del601)/10.0
                            
                            do j = start_line+2,line_num
                                del601=del602
                                read(fileid_del,*) doy2,hour2,minute2,del602
                                write(*,*) doy2,hour2,minute2,del602
                                total_delay=total_delay+5*0.5*(del601+del602)
                            enddo
                            read(fileid_del,*) doy1,hour1,minute1,del601
                            write(*,*) doy1,hour1,minute1,del601
                            total_delay=total_delay+mod(minute_of_year,5)*del602+(mod(minute_of_year,5)*mod(minute_of_year,5))*(del601-del602)/10.0
                            write(fileid_output,"(I6,I7,2(I10),(F12.5),3(I4))",advance='no') incident_id,TARGET_STATION,start_moy,minute_of_year,total_delay,sever(incident_id),pri(incident_id),impact(incident_id)
                            write(fileid_output,*)
                            close(fileid_del)
                            !read(*,*)
                        endif
                    endif
                    close(fileid_occ)
                    exit
                endif
                target_station=target_station-1
                    
            
        enddo
        
        
    enddo
    close(fileid_inc)
    close(fileid_output)
    deallocate(station_mp,station_id)
    end program OCCUPANCY_DETECT

