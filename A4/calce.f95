! Forest Fire Weather Index (FFWI) Calculator
! Author: Conor Roberts
! Date: 01/04/2022
! Based on the work of C.E. Van Wagner & T.L. Pickett

! Declare procedure keepe that saves a string to a file
! Inputs:
!   filename: name of file to save to
!   string: string to save
subroutine keepe(string,file_name)
    integer :: read_file_status

    write(2,*) string
 end subroutine

program calce
    ! Declare variables
    character(len=100) :: file_name=""
    integer :: i=0,n=0,tmp=0,carry=0
    real :: m = 4
    logical :: file_exists = .false.
    integer, dimension(12) :: coef
    real :: test = (n + 1) * 2.30258509
 
    ! Get filename from stdin
    print *,"Please enter the name of the input file: "
    read *, input_file_name

    ! Get number of digits
    print *,"Please enter the name of the input file: "
    read "(I10)", n

  
    ! Check if output file exists
    inquire(file=file_name, exist=file_exists)
 
    ! If file does not exist, open as old file
    if (file_exists) then
        open(unit=1,file=file_name,status='old',action="write")
    else
        open(unit=1,file=file_name,status='new',action="write")
    end if
 

    
    do
        if ((m * (log (m) - 1.0) + 0.5 * log (6.2831852 * m)) >= test) then
            call exit
        end if

        m = m + 1
    end do
        
    for (int i = 0; i < m + 1; i++)
        coef[i] = 1;

    char *arr = (char *)calloc(n + 2, 1);
    arr[0] = '2';
    arr[1] = '.';

    for (int i = 0; i < n; i++)
    {
        int carry = 0;
        for (int j = m; j > 1; j--)
        {
            int tmp = coef[j] * 10 + carry;
            carry = tmp / j;
            coef[j] = tmp - carry * j;
        }
        arr[i + 2] = ((int)carry) + 48;
    }
 
    keepe(arr, file_name)
 end program calce
 