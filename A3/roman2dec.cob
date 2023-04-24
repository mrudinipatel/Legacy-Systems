*> Name: Mrudini Patel (1141712)
*> Course: CIS*3190 A3
*> Date: Friday March 24, 2023

identification division.
program-id. roman2dec.
environment division.
input-output section.

file-control.
      select std-input assign to keyboard.
      select std-output assign to display.
      select input-file assign to filename
           organization is line sequential.

data division.
file section.

fd std-input.
      01 stdin-record pic x(80).
      
fd std-output.
      01 stdout-record pic x(80).

fd  input-file.
      01 input-file-exist pic x(20). 

working-storage section.
      01 eof-switch pic a(1).
      77 file-exists pic 9(1) value 0.
      01 filename pic x(64) VALUE ' '.
      77 input-entered pic x(30) value " ".

      77 iter  pic s99 usage is computational.
      77 temp pic s9(8) usage is computational.
      77 whitespaces pic 9(4).
      77 len pic 9(2).

      77 is-roman pic s99 usage is computational.
      77 prev pic s9(8) usage is computational.
      77 sum1 pic 9(8) usage is computational.
      77 dec pic s9(4) usage is computational.

procedure division.
      open input std-input, output std-output. *> This allows basic stdin/stdout
    
      display "----------------------------------------".
      display "       ROMAN NUMBER EQUIVALENTS".
      display "----------------------------------------".
      display "ROMAN NUMBER         " "        DEC. EQUIV.".
      display "----------------------------------------".

      perform until input-entered is equal to 't' or is equal to 'T'
          display " "
          display "Enter a roman numeral OR a .txt file OR t/T to terminate program:"
          read std-input into input-entered
          display " "
        
          *> Reset variables for each loop
          move 0 to file-exists
          move 0 to iter

          move 0 TO whitespaces 
          move 0 to len                                    
       
          *> Calculating the length of the string without any spaces
          *> Bottom 2 lines from: https://iamamainframer.blogspot.com/2021/06/how-to-find-exact-length-of-string.html
          inspect function reverse (input-entered) tallying whitespaces for leading space
          subtract whitespaces from length of input-entered giving len
        
          *> If input has a '.', then it is considered to be a file
          *> And, thus, flag is incremented
          perform varying iter from 1 by 1 until iter > len
              if input-entered(iter:1) = '.'
                  move 1 to file-exists
              end-if
          end-perform

          *> When input is file we read it until eof is reached
          if file-exists is equal to 1
              move 0 to len
              move 'N' to eof-switch
              move input-entered to filename
              open input input-file

                 perform until eof-switch = 'Y'
                     read input-file
                     at end move 'Y' to eof-switch not at end
                     
                         *> Again, set/reset our variables
                         move input-file-exist to input-entered
                         move 0 to len
                         move 0 TO whitespaces                                     
                         move 0 to iter 

                         *> Again, calculating the length of the string minus spaces (same resource as above)
                         inspect function reverse (input-entered) tallying whitespaces for leading space
                         subtract whitespaces from length of input-entered giving len
                    
                         *> Converting all letters to uppercase to avoid discreptancies/meet suggested output layout
                         move function upper-case(input-entered) to input-entered 

                         if len is equal to 0
                             *> If empty lines exist in input file, print empty lines back
                             display " "
                         else
                             *> Calling on conversion subprogram
                             perform conversion
                         end-if
                      end-read
                   end-perform
              close input-file
          else if file-exists is equal to 0
              *> Case where input is a roman numeral, we convert all letters to uppercase
              move function upper-case(input-entered) TO input-entered 

              *> Calling on conversion subprogram
              perform conversion
          end-if
      end-perform.
stop run.

conversion.
      *> This function is a modification of the provided sample code in the A3 outline pdf.
      *> I have removed the file output functionality and modified certain features to resemble
      *> the modern cobol language. I have implemented the logical flow of Figure 2 diagram.

      *> Reset relevant variables
      move 0 to is-roman.
      move 0 to sum1.
      move 1001 to prev.

      perform varying iter from 1 by 1 until iter is greater than len
          if input-entered(iter:1) is equal to 'I'
              move 1 to dec
              compute sum1 = sum1 + dec

              if prev < dec 
                  compute sum1 = sum1 - 2 * prev
              end-if

              move dec to prev
          else if input-entered(iter:1) is equal to 'V'
              move 5 to dec
              compute sum1 = sum1 + dec

              if prev < dec 
                  compute sum1 = sum1 - 2 * prev
              end-if

              move dec to prev
          else if input-entered(iter:1) is equal to 'X'
              move 10 to dec
              compute sum1 = sum1 + dec

              if prev < dec 
                  compute sum1 = sum1 - 2 * prev
              end-if

              move dec to prev
          else if input-entered(iter:1) is equal to 'L'
              move 50 to dec
              compute sum1 = sum1 + dec

              if prev < dec 
                  compute sum1 = sum1 - 2 * prev
              end-if

              move dec to prev
          else if input-entered(iter:1) is equal to 'C'
              move 100 to dec
              compute sum1 = sum1 + dec

              if prev < dec 
                  compute sum1 = sum1 - 2 * prev
              end-if

              move dec to prev
          else if input-entered(iter:1) is equal to 'D'
              move 500 to dec
              compute sum1 = sum1 + dec

              if prev < dec 
                  compute sum1 = sum1 - 2 * prev
              end-if

              move dec to prev
          else if input-entered(iter:1) is equal to 'M'
              move 1000 to dec
              compute sum1 = sum1 + dec

              if prev < dec 
                  compute sum1 = sum1 - 2 * prev
              end-if
              
              move dec to prev
          else
              *> Flagging character as a "non-roman numeral" digit
              move 1 to is-roman
          end-if
      end-perform.

      *> Displaying output to terminal (instead of an output file)
      if is-roman is equal to 0
          display input-entered sum1
      else if input-entered is equal to 't' or is equal to 'T'
          display "Goodbye."
      else if is-roman is equal to 1
          display input-entered "Illegal roman numeral"
      end-if.

