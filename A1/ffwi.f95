program ffwi
    use FFWIndices

    ! Declare variables
    character(len=100) :: file_name=""
    integer, dimension(12) :: lmon
    real, dimension(12) :: el,fl
    real :: FO,PO,DOT,NDAYS
    integer :: i=0,j=0,M=0

    ! Get filename from stdin
    print *,"Please enter the filename of the file containing the FFW indices: "
    read *,file_name

    ! Open file (corresponding to filename) and month data
    open(unit=1,file=file_name,status='old')
    do i=1,12
        read(1,*) lmon(i),el(i),fl(i)
    end do

    ! Read initial values
    ! READS INITIAL VALUES OF FFMC, DMC, DC, STARTING MONTH AND NUMBER OF DAYS OF DATA STARTING MONTH.
    read(1,*) FO,PO,DOT,M,NDAYS

    do j=m,12
        if(j==m) then
            IDAYS=LMON(J)-NDAYS+1
        else
            IDAYS=1
        end if
    end do
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

    ! Calculate FFMC

    
   
    ! Print out headers
    print *,"  DATE  TEMP  RH   WIND  RAIN   FFMC   DMC   DC   1   ISI   BUI   FWI"
    
    close(1)
end program ffwi