! Assignment 1
! Name: Mrudini Patel
! Student ID: 1141712
! Course: CIS*3190 (DE01) - W23
! Date: Feb 4, 2023

module lexicon
    implicit none
    contains
        subroutine buildlexicon(dict)
            ! Reads dict2.txt (assuming it's in the same directory), and stores each word
            ! at an index in array 'dict'. It then closes file and returns this array.
            integer :: i, iostat
            character(len=32), dimension(100000), intent(inout) :: dict
            
            open(unit=10, file="dict2.txt", status='old', action='read', iostat=iostat)

            do i = 1, size(dict)
                read (10, *, iostat=iostat) dict
            end do

            close(10)
        end subroutine buildlexicon

        subroutine findlex(status)
            ! This is a dummy function because I ended up doing everything inside findAnagram
            ! and permute() itself. Therefore, this function just returns true.
            logical, intent(inout) :: status
            status = .true.
        end subroutine findlex
end module lexicon
