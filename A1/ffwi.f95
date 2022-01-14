program ffwi
    use FFWIndices

    ! Declare variables
    character(len=100) :: file_name=""
    integer, dimension(12) :: month_lengths
    real, dimension(12) :: dmc_factors,dc_factors
    integer :: i=0,j=0

    ! Get filename from stdin
    print *,"Please enter the filename of the file containing the FFW indices: "
    read *,file_name

    ! Declare formats
    write(*,104)
104 format(2X,'PROGRAM NO.: F-4')
100 format(I2,F4.1,F4.1)
101 format(F4.1,I4,I4,F4.1)
102 format(F4.1,F4.1,F5.1,I2,I2)
103 format(10(/),1X,'  DATE  TEMP  RH   WIND  RAIN   FFMC   DMC   DC   1   ISI   BUI   FWI'/)

    ! Open file (corresponding to filename) and month data
    open(unit=1,file=file_name,status='old')

    ! Read month lengths, dmc daylength factors, dc daylength factors
    do i=1,12
        read(1,100) month_lengths(i),dmc_factors(i),dc_factors(i)
    end do

    read(1,102) fo,po,dot,m,bdays
    ! READ(1,102) FO,PO,DOT,M,NDAYS



    ! Print header
    ! print *,103

    ! Print data
    ! do i=days_in_starting_month,month_lengths(starting_month)

    ! end do


        ! WRITE(*,1001) J,I,TX,IH,IW,RAIN,IFFM,IDMC,IDC,ISI,IBUI,IFWI
! 1001  FORMAT(1X,2I3,F6.1,I4,I6,F7.1,6I6)
!    end do

    ! Read initial values
    ! READS INITIAL VALUES OF FFMC, DMC, DC, STARTING MONTH AND NUMBER OF DAYS OF DATA STARTING MONTH.
    ! read(1,*) FO,PO,DOT,M,NDAYS

    ! do j=m,12
    !     if(j==m) then
    !         IDAYS=LMON(J)-NDAYS+1
    !     else
    !         IDAYS=1
    !     end if
    ! end do
    ! NN=LMON(J)
    ! L=0
    ! do i=IDAYS,NN
    !     L=L+1
    ! end do
    ! READ(*,*) T,IH,IW,R
    ! IF(L/=1) GO TO 301
    ! WRITE(*,1002)
    ! TX=T
    ! H=IH
    ! W=IW
    ! RAIN=R

    ! integer :: current_day=1
    ! integer :: current_f = 0

    ! Previous day's F becomes F0
    ! If R0>0.5, calculate F(R0) 
    ! if (current_day>1) then
        ! days(current_day-1) = current_f

    ! Calculate FFMC

    
   
    ! Print out headers
    ! print *,"  DATE  TEMP  RH   WIND  RAIN   FFMC   DMC   DC   1   ISI   BUI   FWI"
    
    close(1)
end program ffwi