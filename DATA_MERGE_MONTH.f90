!CHECK!
!  DATA_MERGE_MONTH.f90
!
!  FUNCTIONS:
!  DATA_MERGE_MONTH - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: DATA_MERGE_MONTH
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************
module variables
    integer ::fileid_sta=11
    integer ::fileid_output=13
    integer ::fileid_pro=15
    character(len=60) ::filename_pro
    character(len=30) ::filename_output
    character(len=30) :: filename_sta="STA_ID.txt"
    character(len=60) ::filepath
    !character(len=30) ::traffic_feature(23)=(/"FlowPL","Occupancy","Speed","TRUCKFLOW","TRUCKPROP","VHT","VMT","TRUCKVMT","TRUCKVHT","Q","TTI","DEL35","DEL40","DEL45","DEL50","DEL55","DEL60","LP35","LP40","LP45","LP50","LP55","LP60"/)
    ! THERE ARE 23 TRAFFIC FEATURES
    character(len=30) ::traffic_feature(3)
    integer i,j,k
    integer total_sta_num
    ! TOTAL NUMBER OF STATIONS
    integer ::sta_id(100)
    real ::sta_mp(100)
    character(len=2) :: month_str
    character(len=10) ::sta_id_str
    integer temp1,temp3,temp2
    real temp4
    integer status_1
end module
    program DATA_MERGE_MONTH
    use variables
    implicit none
        traffic_feature(1)="Flow"
        traffic_feature(2)="Lane"
        traffic_feature(3)="Speed"
    open (fileid_sta,file=filename_sta)
    read (fileid_sta,*) total_sta_num
!    total_sta_num=81
    ! READ IN STATION INFORMATION
    do i = 1, total_sta_num
        read(fileid_sta,*) sta_mp(i),sta_id(i)
        write(*,*) sta_mp(i),sta_id(i)
    enddo
    read(*,*)
    ! CHOOSE MERGE FEATURES

!    do i = 1, 17
     do i = 1,3
        ! LOCATE THE STORAGE PATH
        filepath="I_15_"//trim(traffic_feature(i))//"_North/TXT/"
        write(*,*) traffic_feature(i)
        do j = 1, TOTAL_STA_NUM
            write(sta_id_str,"(I8)") sta_id(j)
            filename_output='./output/N_'//trim(adjustl(sta_id_str))//'_'//trim(adjustl(traffic_feature(i)))//'.txt'
            open (fileid_output,file=filename_output)
            write(*,*) trim(filename_output)
            do k = 1,12
                write(month_str,"(I2)") k
                    filename_pro=trim(adjustl(filepath))//trim(adjustl(sta_id_str))//'_'//trim(adjustl(month_str))//'.txt'
                    write(*,*) filename_pro
                    open (fileid_pro,file=filename_pro)
                    do while(.true.)
                    read (fileid_pro,*,iostat=status_1) temp1, temp2, temp3, temp4
                    if (status_1.lt.0) exit
                    write (fileid_output,*) temp1, temp2,temp3,temp4
                    end do
            enddo
            close (fileid_output)
        enddo
    enddo
    ! Variables

    ! Body of DATA_MERGE_MONTH


    end program DATA_MERGE_MONTH
