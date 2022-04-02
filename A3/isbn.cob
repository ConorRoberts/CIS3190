identification division.
program-id. isbn.

environment division.
input-output section.
file-control.

    *> Define where we're reading into.
    select inputFile assign to dynamic fileName
    organization is line sequential
    file status is fileStatus.

data division.
file section.

*> Structure of the file.
fd inputFile.
    01 isbn.
        02 str pic X occurs 10 times.

working-storage section.

    *> Variables for file IO
    01 eof pic 9 value 1.
    01 fileStatus pic 9(2).
    01 fileName pic x(50).
       
	*> Helper variables for constructing string later
    01 isValidName pic 9 value 0.
    01 isValidValue pic 9 value 0.

	*> Variables used for calculation later
    01 i pic 99.
    01 isbnSum pic 9(4) value 0.
    01 checkDigit pic 9(2).
    01 mod pic 9(2).
    01 isbnRemainder pic 9(2).
    01 val pic 9.

	*> The string we are going to output later
    01 outputString.
        02 isbnValue pic X occurs 9 times.
        02 isbnCheck pic X.
        02 isbnStatus   pic x(35).

procedure division.
    *> Read file names from stdin until we get a valid one.
    perform readISBN
        until isValidName = 1.
stop run.

readISBN.

    *> Get file name from stdin.
    display "File name:".
    accept fileName.

	*> Open file
    open input inputFile.

    *> Check if the file is open
    if fileStatus = 00
       
	    *> Assume we have a valid file name
        move 1 to isValidName
       
	    *> Run the code until the end of file is hit
        perform until eof = 0
			read inputFile
			    *> Signal that we have hit the end of the file
				at end move zero to eof
			end-read

			*> If we are not at the end of file
			if eof is not equal to zero
				perform isValid
				perform makeOutputString
				display outputString
			end-if
		end-perform

        display "Done."
    else
        display "Error - Could not open file."
    end-if.

    close inputFile.

isValid.
    move 1 to isValidValue.

    *> Loop through every character in the ISBN string and checks if it's valid
    perform varying i from 1 by 1 until i = 11

	    *> Copy to the value that we print later
        move str(i) to isbnValue(i)

		*> Check if it's not a number or not x/X. Then we know it's invalid.
        if str(i) is not numeric and str(i) is not = 'X' and str(i) is not = 'x'
            move 0 to isValidValue
    end-perform.

    *> Only perform checkSum if we have a valid value
    if isValidValue = 1
        perform checkSum.

    move 0 to isbnSum.

checkSum.

    *> Loop through every character in the ISBN string and add it to the sum
    perform varying i from 10 by -1 until i=0
        move isbnValue(10 - i) to val
		compute isbnSUM = i * val
    end-perform.
    
	*> Compute the check digit
    move str(10) to isbnCheck
    if isbnCheck is equal to 'X' or isbnCheck is equal to 'x'
        move 10 to checkDigit
    else
        move isbnCheck to checkDigit.

    *> Compute the remainder
    divide isbnSum by 11 giving mod remainder isbnRemainder.
    compute isbnRemainder = 11 - isbnRemainder.

	*> Special case where remainder is 11 (should be 0)
    if isbnRemainder = 11
        move 0 to isbnRemainder.

makeOutputString.
	if isValidValue = 0
        move " incorrect, contains a non-digit." to isbnStatus
	else
		if checkDigit = isbnRemainder
			move " correct, and valid" to isbnStatus
		else
			move " correct, but not valid" to isbnStatus.

