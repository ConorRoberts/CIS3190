program ffwi
    use FFWIndices

    ! Declare variables
    character(len=100) :: file_name=""
    integer, dimension(12) :: month_lengths
    real, dimension(12) :: dmc_factors,dc_factors
    integer :: i=0,days_left=0,current_month=0,current_day=0
    real :: current_dmc,previous_dmc,current_ffmc,previous_ffmc,current_dc,previous_dc
    real :: temperature,rainfall,previous_moisture
    integer:: humidity,wind_speed,ffmc,dmc,dc,isi,bui,fwi

    ! Get filename from stdin
    print *,"Please enter the name of the file containing the FFW indices: "
    read *,file_name

    ! Declare formats
    write(*,104)
100 format(I2,F4.1,F4.1)
101 format(F4.1,I4,I4,F4.1)
102 format(F4.1,F4.1,F5.1,I2,I2)
103 format(10(/),1X,'  DATE  TEMP  RH   WIND  RAIN   FFMC   DMC   DC   ISI   BUI   FWI'/)
104 format(2X,'PROGRAM NO.: F-4')
105 format(1X,2I3,F6.1,I4,I6,F7.1,6I6)

    ! Open file (corresponding to filename) and month data
    open(unit=1,file=file_name,status='old')

    ! Read month lengths, dmc daylength factors, dc daylength factors
    do i=1,12
        read(1,100) month_lengths(i),dmc_factors(i),dc_factors(i)
    end do

    read(1,102) current_ffmc,current_dmc,current_dc,current_month,days_left

    previous_dc = current_dc
    previous_dmc = current_dmc
    previous_ffmc = current_ffmc

    ! Print column headers
    write (*,103)

    ! Get initial current day
    current_day = month_lengths(current_month) - days_left+1

    ! Main loop
    do
        read(1,101,END=400) temperature,humidity,wind_speed,rainfall

        ffmc=get_ffmc(temperature,humidity,wind_speed,previous_moisture)
        dmc=0
        dc=0
        isi=0
        bui=0
        fwi=0
        
        ! Print data
        write(*,105) current_month,current_day,temperature,humidity,wind_speed,rainfall,ffmc,dmc,dc,isi,bui,fwi

        previous_ffmc = ffmc
        previous_dc = dc
        previous_dmc = dmc
        
        ! Check if we are beyond the end of the current month
        if (current_day==month_lengths(current_month)) then
            ! Print header
            write (*,103)

            ! Increment month and reset day
            current_month = current_month + 1
            current_day = 1
        else
            ! Increment current day
            current_day = current_day + 1
        end if
    end do
    
400 close(1)

end program ffwi