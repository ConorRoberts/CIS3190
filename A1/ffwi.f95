program ffwi
    use FFWIndices

    ! Declare variables
    character(len=100) :: file_name=""
    integer, dimension(12) :: month_lengths
    real, dimension(12) :: dmc_factors,dc_factors
    integer :: i=0,days_left=0,current_month=0,current_day=0
    real :: temperature,rainfall
    integer:: humidity,wind_speed,H,W
    real :: starting_ffmc,ffmc,dmc,dc,isi,bui,fwi
    logical :: should_print_header = .TRUE.
    real :: T,R

    ! Get filename from stdin
    print *,"Please enter the name of the file containing the FFW indices: "
    read *,file_name

    ! Declare formats
100 format(I2,F4.1,F4.1)
101 format(F4.1,I4,I4,F4.1)
102 format(F4.1,F4.1,F5.1,I2,I2)
105 format(1X,2I3,F6.1,I4,I6,F7.1,6I6)

    ! Open file (corresponding to filename) and month data
    open(unit=1,file=file_name,status='old',ERR=404)

    ! Read month lengths, dmc daylength factors, dc daylength factors
    do i=1,12
        read(1,100,END=400) month_lengths(i),dmc_factors(i),dc_factors(i)
    end do

    read(1,102,END=400) starting_ffmc,starting_dmc,starting_dc,current_month,days_left
    ffmc = starting_ffmc
    dmc = starting_dmc
    dc = starting_dc

    ! Get initial current day
    current_day = month_lengths(current_month) - days_left+1

    ! Main loop
    do
        read(1,101,END=400) temperature,humidity,wind_speed,rainfall

        ! Assign descriptive variables to shorter strings to respect column limit
        W = wind_speed
        T = temperature
        R = rainfall
        H = humidity

        ffmc=get_ffmc(temperature,humidity,wind_speed,rainfall,ffmc)
        dmc=get_dmc(temperature,humidity,rainfall,dmc,dmc_factors(current_month))
        dc=get_dc(temperature,rainfall,dc,dc_factors(current_month))
        isi=get_isi(W)
        bui=get_bui(dc,dmc)
        fwi=get_fwi(bui,isi)

        if (should_print_header) then
            call print_header()
            should_print_header = .FALSE.
        end if
        
        ! Print data
        write(*,105) current_month,current_day,T,H,W,R,nint(ffmc),nint(dmc),nint(dc),nint(isi),nint(bui),nint(fwi)

        previous_ffmc = ffmc
        previous_dc = dc
        previous_dmc = dmc
        
        ! Check if we are beyond the end of the current month
        if (current_day==month_lengths(current_month)) then
            ! Print header
            should_print_header = .TRUE.

            ! Increment month and reset day
            current_month = current_month + 1
            current_day = 1
        else
            ! Increment current day
            current_day = current_day + 1
        end if

    end do
    
404 print *,"Error: File not found"
400 close(1)

end program ffwi