! Author: Conor Roberts
! Date: 01/04/2022
! Description: This program generates e to the specified number of digits.

subroutine keepe(string)
    character(len=*), intent(in) :: string
    write(2,"(a)",advance="no") string
end subroutine

program calce
    ! Declare variables
    character(len=100) :: file_name=""
    integer :: n=0,tmp=0,carry=0
    real :: m = 4
    logical :: file_exists = .false.
    real :: test = 0.0
    character (len=:), ALLOCATABLE :: arr
    integer,dimension(:), ALLOCATABLE :: coef
    
    ! Get filename from stdin
    print *,"Enter file name: "
    read *, file_name
    
    ! Get number of digits
    print *,"Enter number of digits: "
    read "(I10)", n
    
    ! Check if output file exists
    inquire(file=file_name, exist=file_exists)
    
    ! If file does not exist, open as old file
    if (file_exists) then
        open(unit=2,file=file_name,status='old',action="write")
    else
        open(unit=2,file=file_name,status='new',action="write")
    end if
    
    test = (n + 1) * 2.30258509
    do while ((m * (log (m) - 1.0) + 0.5 * log (6.2831852 * m)) < test)
        m = m + 1
    end do

    allocate( coef(nint(m)+1) )

    do i = 1, nint(m)+2
        coef(i) = 1;
    end do

    allocate( character(len=n+1) :: arr )

    arr(1:2) = "2."
    do i = 3, n+1
        carry = 0
        do j = nint(m), 2, -1
            tmp = coef(j) * 10 + carry;

            carry = tmp / j;

            coef(j) = tmp - carry * j;
        end do

        arr(i:i) = char(carry + 48)
    end do

    call keepe(arr)

    close (2,status="keep")

    DEALLOCATE(coef)
    DEALLOCATE(arr)
 end program calce
 