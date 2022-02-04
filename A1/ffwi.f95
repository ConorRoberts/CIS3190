! Forest Fire Weather Index (FFWI) Calculator
! Author: Conor Roberts
! Date: 31/01/2022
! Based on the work of C.E. Van Wagner & T.L. Pickett

program ffwi
   use FFWIndices

   ! Declare variables
   character(len=100) :: input_file_name="",do_file_output="",output_file_name=""
   integer, dimension(12) :: month_lengths
   real, dimension(12) :: dmc_factors,dc_factors
   integer :: i=0,days_left=0,current_month=0,current_day=0
   real :: temperature,rainfall, T, R
   integer:: humidity,wind_speed,H,W, file_read_status
   real :: starting_ffmc,ffmc,dmc,dc,isi,bui,fwi
   logical :: should_print_header = .TRUE., file_exists

   ! Declare formats
   character(len=30) :: factors_format="(I2,F4.1,F4.1)"
   character(len=30) :: main_data_format="(F4.1,I4,I4,F4.1)"
   character(len=30) :: output_format="(1X,2I3,F6.1,I4,I6,F7.1,6I6)"
   character(len=30) :: start_data_format="(F4.1,F4.1,F5.1,I2,I2)"

   ! Get filename from stdin
   print *,"Please enter the name of the input file: "
   read *,input_file_name

   ! Check if input file exists
   inquire(file=input_file_name, exist=file_exists)

   ! If file does not exist, close program
   if (file_exists) then
      open(unit=1,file=input_file_name,status="old",action="read")
   else
      print *,"Error - File not found"
      call exit
   end if

   ! Check whether or not we're going to do file output
   do while(do_file_output /= "yes" .and. do_file_output /= "no")
      print *,"Would you like to output to a file? (yes/no): "
      read *,do_file_output
   end do

   ! We are outputting to file. Get new file name and open it.
   if (do_file_output=="yes") then
      print *,"What should this file be named?: "
      read *,output_file_name

      ! Check if output file exists
      inquire(file=output_file_name, exist=file_exists)

      ! If file does not exist, close program
      if (file_exists) then
         print *,"Error - File already exists"
         call exit
      else
         open(unit=2,file=output_file_name,status='new',action="write")
      end if
   end if

   ! Read month lengths, dmc daylength factors, dc daylength factors
   do i=1,12
      read(1,factors_format,IOSTAT=file_read_status) month_lengths(i),dmc_factors(i),dc_factors(i)
      call check_file_read_status(file_read_status)
   end do

   read(1,start_data_format,IOSTAT=file_read_status) starting_ffmc,starting_dmc,starting_dc,current_month,days_left
   call check_file_read_status(file_read_status)

   ffmc = starting_ffmc
   dmc = starting_dmc
   dc = starting_dc

   ! Get initial current day
   current_day = month_lengths(current_month) - days_left+1

   ! Main loop
   do
      read(1,main_data_format,IOSTAT=file_read_status) temperature,humidity,wind_speed,rainfall
      call check_file_read_status(file_read_status)

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
         if (do_file_output=="yes") then
            call print_header_to_stream(2)
         else
            call print_header()
         end if
         should_print_header = .FALSE.
      end if

      ! Print data
      if (do_file_output=="yes") then
         write(2,output_format) current_month,current_day,T,H,W,R,nint(ffmc),nint(dmc),nint(dc),nint(isi),nint(bui),nint(fwi)
      else
         write(*,output_format) current_month,current_day,T,H,W,R,nint(ffmc),nint(dmc),nint(dc),nint(isi),nint(bui),nint(fwi)
      end if

      ! Track previous values
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

   ! Close input file
   close (1,status="keep")

   ! Check if output file should be closed
   if (do_file_output=="yes") then
      close (2,status="keep")
   end if

end program ffwi
