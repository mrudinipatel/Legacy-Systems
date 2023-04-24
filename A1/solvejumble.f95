! Assignment 1
! Name: Mrudini Patel
! Student ID: 1141712
! Course: CIS*3190 (DE01) - W23
! Date: Feb 4, 2023

program wordjumble
    use lexicon
    implicit none
    logical :: dummy

    call inputJumble()
    call findlex(dummy)
    print *, NEW_LINE('a'), NEW_LINE('a'), "Program ended"

end program wordjumble


subroutine inputJumble()
    ! This subroutine retrieves input on number of words. It then checks if number of words
    ! is valid (>= 0). If it is, it prompts the user that many times to enter words. These words
    ! are then stored inside an array and passed into generateAnagram.
    integer :: numberWords, i
    character(len=32), dimension(50):: words

    write (*,'(a,$)') 'How many jumbled words? '
    read (*,*) numberWords

    if (numberWords <= 0) then
        write (*,'(a,$)') 'Please run program again to enter a valid number.'
    else
        print *, NEW_LINE('a')
        write(*, '(a, i0, a)') "Enter the ", numberWords, " jumbled words:"

        do i = 1, numberWords
            write (*,'(a,$)') ">"
            read (*,*) words(i)
        end do
    end if

    call generateAnagram(words, numberWords)
end subroutine inputJumble

subroutine generateAnagram(arr, numWords)
    integer :: numWords, i, j, count, newCount, k, length, result, countI, newCountI
    character(len=32), dimension(50) :: arr
    character(len=32), dimension(100) :: foundWords, foundWordsI
    logical :: duplicate, duplicateI
    character(len=100) :: input, circledWord
    character(len=1) :: solve

    ! Initializing array indexes to null (mainly to help with error handling cases below)
    do i = 1, size(foundWords)
        foundWords(i) = ''
    end do

    do i = 1, size(foundWordsI)
        foundWordsI(i) = ''
    end do

    ! Calls permute() to find all permutations/matches for each inputted word
    count = 0
    do i = 1, numWords
        call permute(arr(i), 1, len_trim(arr(i)), count, foundWords)
    end do

    ! Handle if no words were solved
    if (foundWords(1) .eq. '') then
        print *, NEW_LINE('a'), NEW_LINE('a'), "No jumble(s) were found in the dictionary. Please run again."
        return
    end if

    ! Remove any duplicates in foundWords array and shift words over an index
    newCount = 0
    do i = 1, count
        duplicate = .false.
        
        do j = 1, newCount
            if (foundWords(i) == foundWords(j)) then
                duplicate = .true.
                exit
            end if
        end do

        if (.not. duplicate) then
            newCount = newCount + 1
            foundWords(newCount) = foundWords(i)
        end if
    end do

    ! Printing out the solved jumbles (this part is a little bit different from provided output in outline)
    print *, NEW_LINE('a'), NEW_LINE('a'), "The following jumbles have been solved:"
    do i = 1, newCount
        print *, foundWords(i)
    end do

    ! Asking user if they would like to continue and solve the word jumble puzzle
    print *, NEW_LINE('a')
    write (*,'(a,$)') "Solve word jumble puzzle (y/Y for yes, any other character for no)? "
    read (*,*) solve

    ! If y/Y (yes) then prompt them to enter circled letters and call permute() again.
    ! If no, print exiting message and exit program
    if (solve .eq. 'y' .or. solve .eq. 'Y') then
        result = 0
        print *, NEW_LINE('a'), NEW_LINE('a'), "Select circled letters for the following solved jumbles:"
        
        do i = 1, newCount
            write(*,'(a,$)') foundWords(i), ": "
            read (*,'(a)') input
            
            length = len(input)

            do k = 1, length
                    if ( .not. (input(k:k) .eq. ' ' .or. input(k:k) .eq. '\t') ) then
                        result = result + 1
                        circledWord(result:result) = input(k:k)
                    end if
            end do
        end do

        circledWord = circledWord(1:result)
        write (*,'(a,$)') "Jumbled word:", circledWord
        
        countI = 0
        call permute(circledWord, 1, len_trim(circledWord), countI, foundWordsI)

        if (foundWordsI(1) .eq. '') then
            print *, NEW_LINE('a'), NEW_LINE('a'), "Jumbled word was not found in dictionary. Please run again."
            return
        end if

        newCountI = 0
        do i = 1, countI
            duplicateI = .false.
            
            do j = 1, newCountI
                if (foundWordsI(i) == foundWordsI(j)) then
                    duplicateI = .true.
                    exit
                end if
            end do

            if (.not. duplicateI) then
                newCountI = newCountI + 1
                foundWordsI(newCountI) = foundWordsI(i)
            end if
        end do

        print *, NEW_LINE('a'), NEW_LINE('a'), "Solved word:"
        do i = 1, newCountI
            print *, foundWordsI(i)
        end do
    else
        print *, "Since you did not enter y/Y, exiting program. See you again."
        return
    end if
end subroutine generateAnagram

subroutine swap (x, y)
    ! This subroutine is a helper function created to easily swap characters
    ! to generate permutations for the word passed into the permute() subroutine
    character(len=1), intent(inout) :: x, y
    character(len=1) :: temp

    temp = x
    x = y
    y = temp
end subroutine swap

recursive subroutine permute(word, index, n, count, foundWords)
    ! This function implements the recursive algorithm found at the following URL (written by ):
    ! https://craftofcoding.wordpress.com/2023/01/23/recursion-permutations-by-interchange/
    
    integer :: index, n, j
    integer, intent(inout) :: count
    character(len=32), intent(in) :: word
    character(len=32) :: tempWord, match
    logical :: result
    character(len=32), dimension(100), intent(inout) :: foundWords

    tempWord = word
    tempWord = adjustl(tempWord)

    if (index == n) then
        ! For each permutation, search in the dictionary by passing the word into the 
        ! findAnagram subroutine. Return match and count back to the generateAnagram() subroutine.
        call findAnagram(tempWord, match, result)
        
        if (result) then
            count = count + 1
            foundWords(count) = match
        end if
    else
        do j = index, n
            tempWord(index:index) = word(j:j)
            tempWord(j:j) = word(index:index)
            
            call permute(tempWord, index+1, n, count, foundWords)
            
            tempWord(j:j) = word(j:j)
            tempWord(index:index) = word(index:index)
        end do
    end if
end subroutine permute

subroutine findAnagram(permWord, wordFound, found)
    ! This subroutine calls on buildlexicon from lexicon module to conduct a 
    ! linear search between permWord and the dict2.txt dictionary. If found, it 
    ! returns true and the found word back to permute() subroutine which returns it
    ! back to the generateAnagram() subroutine. Linear search is done after converting
    ! permWord and lex to lowercase using the built-in adjustl() function.

    use lexicon
    character(len=32), intent(inout) :: permWord
    character(len=32), dimension(100000), SAVE :: lex
    character (len=32), intent(out) :: wordFound
    logical, intent(out) :: found
    integer :: i

    found = .false.
    wordFound = ''

    call buildlexicon(lex)
    permWord = adjustl(permWord)

    do i = 1, size(lex)
        if(permWord .eq. adjustl(lex(i))) then
            found = .true.
            wordFound = lex(i)
            return
        end if
    end do
end subroutine findAnagram

